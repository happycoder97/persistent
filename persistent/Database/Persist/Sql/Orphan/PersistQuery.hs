{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistQuery
    ( deleteWhereCount
    , updateWhereCount
    , decorateSQLWithLimitOffset
    , (~~.), (~~*.), (!~~.), (!~~*.)
    ) where

import Database.Persist hiding (updateField)
import Database.Persist.Sql.Util (
    entityColumnNames, parseEntityValues, isIdField, updatePersistValue
  , mkUpdateText, commaSeparated)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Util (dbIdColumns)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid (Monoid (..), (<>))
import Data.Int (Int64)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, withReaderT)
import Control.Exception (throwIO)
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.ByteString.Char8 (readInteger)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.List (transpose, inits, find)

-- orphaned instance for convenience of modularity
instance PersistQueryRead SqlBackend where
    count filts = do
        conn <- ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = mconcat
                [ "SELECT COUNT(*) FROM "
                , connEscapeName conn $ entityDB t
                , wher
                ]
        withRawQuery sql (getFiltsValues conn filts) $ do
            mm <- CL.head
            case mm of
              Just [PersistInt64 i] -> return $ fromIntegral i
              Just [PersistDouble i] ->return $ fromIntegral (truncate i :: Int64) -- gb oracle
              Just [PersistByteString i] -> case readInteger i of -- gb mssql
                                              Just (ret,"") -> return $ fromIntegral ret
                                              xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
              Just xs -> error $ "count:invalid sql  return xs["++show xs++"] sql["++show sql++"]"
              Nothing -> error $ "count:invalid sql returned nothing sql["++show sql++"]"
      where
        t = entityDef $ dummyFromFilts filts

    selectSourceRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        (limit, offset, orders) = limitOffsetOrder opts

        parse vals = case parseEntityValues t vals of
                       Left s -> liftIO $ throwIO $ PersistMarshalError s
                       Right row -> return row
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " <> T.intercalate "," ords
        cols = commaSeparated . entityColumnNames t
        sql conn = connLimitOffset conn (limit,offset) (not (null orders)) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            ]

    selectKeysRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        t = entityDef $ dummyFromFilts filts
        cols conn = T.intercalate "," $ dbIdColumns conn t


        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = connLimitOffset conn (limit,offset) (not (null orders)) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            ]

        (limit, offset, orders) = limitOffsetOrder opts

        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " <> T.intercalate "," ords

        parse xs = do
            keyvals <- case entityPrimary t of
                      Nothing ->
                        case xs of
                           [PersistInt64 x] -> return [PersistInt64 x]
                           [PersistDouble x] -> return [PersistInt64 (truncate x)] -- oracle returns Double
                           _ -> return xs
                      Just pdef ->
                           let pks = map fieldHaskell $ compositeFields pdef
                               keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) xs
                           in return keyvals
            case keyFromValues keyvals of
                Right k -> return k
                Left err -> error $ "selectKeysImpl: keyFromValues failed" <> show err
instance PersistQueryRead SqlReadBackend where
    count filts = withReaderT persistBackend $ count filts
    selectSourceRes filts opts = withReaderT persistBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withReaderT persistBackend $ selectKeysRes filts opts
instance PersistQueryRead SqlWriteBackend where
    count filts = withReaderT persistBackend $ count filts
    selectSourceRes filts opts = withReaderT persistBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withReaderT persistBackend $ selectKeysRes filts opts

instance PersistQueryWrite SqlBackend where
    deleteWhere filts = do
        _ <- deleteWhereCount filts
        return ()
    updateWhere filts upds = do
        _ <- updateWhereCount filts upds
        return ()
instance PersistQueryWrite SqlWriteBackend where
    deleteWhere filts = withReaderT persistBackend $ deleteWhere filts
    updateWhere filts upds = withReaderT persistBackend $ updateWhere filts upds

-- | Same as 'deleteWhere', but returns the number of rows affected.
--
-- @since 1.1.5
deleteWhereCount :: (PersistEntity val, MonadIO m, PersistEntityBackend val ~ SqlBackend, IsSqlBackend backend)
                 => [Filter val]
                 -> ReaderT backend m Int64
deleteWhereCount filts = withReaderT persistBackend $ do
    conn <- ask
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else filterClause False conn filts
        sql = mconcat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , wher
            ]
    rawExecuteCount sql $ getFiltsValues conn filts

-- | Same as 'updateWhere', but returns the number of rows affected.
--
-- @since 1.1.5
updateWhereCount :: (PersistEntity val, MonadIO m, SqlBackend ~ PersistEntityBackend val, IsSqlBackend backend)
                 => [Filter val]
                 -> [Update val]
                 -> ReaderT backend m Int64
updateWhereCount _ [] = return 0
updateWhereCount filts upds = withReaderT persistBackend $ do
    conn <- ask
    let wher = if null filts
                then ""
                else filterClause False conn filts
    let sql = mconcat
            [ "UPDATE "
            , connEscapeName conn $ entityDB t
            , " SET "
            , T.intercalate "," $ map (mkUpdateText conn) upds
            , wher
            ]
    let dat = map updatePersistValue upds `Data.Monoid.mappend`
              getFiltsValues conn filts
    rawExecuteCount sql dat
  where
    t = entityDef $ dummyFromFilts filts

fieldName ::  forall record typ.  (PersistEntity record, PersistEntityBackend record ~ SqlBackend) => EntityField record typ -> DBName
fieldName f = fieldDB $ persistFieldDef f

dummyFromFilts :: [Filter v] -> Maybe v
dummyFromFilts _ = Nothing

getFiltsValues :: forall val. (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
               => SqlBackend -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn OrNullNo

data OrNull = OrNullYes | OrNullNo

filterClauseHelper :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> SqlBackend
             -> OrNull
             -> [Filter record]
             -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn orNull filters =
    (if not (T.null sql) && includeWhere
        then " WHERE " <> sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
    filterValueToPersistValues v = map toPersistValue $ either return id v

    combine s fs =
        (T.intercalate s $ map wrapP a, mconcat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (FilterAnd []) = ("1=1", [])
    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=0", [])
    go (FilterOr fs)  = combine " OR " fs
    go (BackendFilter filt) = case filt of
        LikeFilter fld val     ->
            ((if includeTable then ((tn <> ".") <>) else id)
             (connEscapeName conn $ fieldName fld) <> " " <> "LIKE" <> " ?", [PersistText val])
        ILikeFilter fld val     ->
            ((if includeTable then ((tn <> ".") <>) else id)
             (connEscapeName conn $ fieldName fld) <> " " <> "ILIKE" <> " ?", [PersistText val])
        NotLikeFilter fld val     ->
            ((if includeTable then ((tn <> ".") <>) else id)
             (connEscapeName conn $ fieldName fld) <> " " <> "NOT LIKE" <> " ?", [PersistText val])
        NotILikeFilter fld val     ->
            ((if includeTable then ((tn <> ".") <>) else id)
             (connEscapeName conn $ fieldName fld) <> " " <> "NOT ILIKE" <> " ?", [PersistText val])
      where
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromBackendFilter filt

        dummyFromBackendFilter :: SqlLikeFilter record -> Maybe record
        dummyFromBackendFilter _ = Nothing

    go filterArg@(Filter field value pfilter) =
        let t = entityDef $ dummyFromFilts [filterArg]
        in case (isIdField field, entityPrimary t, allVals) of
                 (True, Just pdef, PersistList ys:_) ->
                    if length (compositeFields pdef) /= length ys
                       then error $ "wrong number of entries in compositeFields vs PersistList allVals=" ++ show allVals
                    else
                      case (allVals, pfilter, isCompFilter pfilter) of
                        ([PersistList xs], Eq, _) ->
                           let sqlcl=T.intercalate " and " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        ([PersistList xs], Ne, _) ->
                           let sqlcl=T.intercalate " or " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        (_, In, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " and " (map wrapSql sqls)), concat xxs)
                        (_, NotIn, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " or " (map wrapSql sqls)), concat xxs)
                        ([PersistList xs], _, True) ->
                           let zs = tail (inits (compositeFields pdef))
                               sql1 = map (\b -> wrapSql (T.intercalate " and " (map (\(i,a) -> sql2 (i==length b) a) (zip [1..] b)))) zs
                               sql2 islast a = connEscapeName conn (fieldDB a) <> (if islast then showSqlFilter pfilter else showSqlFilter Eq) <> "? "
                               sqlcl = T.intercalate " or " sql1
                           in (wrapSql sqlcl, concat (tail (inits xs)))
                        (_, BackendSpecificFilter _, _) -> error "unhandled type BackendSpecificFilter for composite/non id primary keys"
                        _ -> error $ "unhandled type/filter for composite/non id primary keys pfilter=" ++ show pfilter ++ " persistList="++show allVals
                 (True, Just pdef, []) ->
                     error $ "empty list given as filter value filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef
                 (True, Just pdef, _) ->
                     error $ "unhandled error for composite/non id primary keys filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef

                 _ ->   case (isNull, pfilter, length notNullVals) of
                            (True, Eq, _) -> (name <> " IS NULL", [])
                            (True, Ne, _) -> (name <> " IS NOT NULL", [])
                            (False, Ne, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " <> "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
                            -- not all databases support those words directly.
                            (_, In, 0) -> ("1=2" <> orNullSuffix, [])
                            (False, In, _) -> (name <> " IN " <> qmarks <> orNullSuffix, allVals)
                            (True, In, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (False, NotIn, 0) -> ("1=1", [])
                            (True, NotIn, 0) -> (name <> " IS NOT NULL", [])
                            (False, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (True, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NOT NULL AND "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            _ -> (name <> showSqlFilter pfilter <> "?" <> orNullSuffix, allVals)
      where
        isCompFilter Lt = True
        isCompFilter Le = True
        isCompFilter Gt = True
        isCompFilter Ge = True
        isCompFilter _ =  False

        wrapSql sqlcl = "(" <> sqlcl <> ")"
        fromPersistList (PersistList xs) = xs
        fromPersistList other = error $ "expected PersistList but found " ++ show other

        orNullSuffix =
            case orNull of
                OrNullYes -> mconcat [" OR ", name, " IS NULL"]
                OrNullNo -> ""

        isNull = any (== PersistNull) allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn <> ".") <>)
                else id)
            $ connEscapeName conn $ fieldName field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" <> T.intercalate "," (map (const "?") x') <> ")"
        showSqlFilter Eq = "="
        showSqlFilter Ne = "<>"
        showSqlFilter Gt = ">"
        showSqlFilter Lt = "<"
        showSqlFilter Ge = ">="
        showSqlFilter Le = "<="
        showSqlFilter In = " IN "
        showSqlFilter NotIn = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s

filterClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
             => Bool -- ^ include table name?
             -> SqlBackend
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

orderClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => Bool -- ^ include the table name
            -> SqlBackend
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x <> " DESC"
        _ -> error "orderClause: expected Asc or Desc, not limit or offset"
  where
    dummyFromOrder :: SelectOpt a -> Maybe a
    dummyFromOrder _ = Nothing

    tn = connEscapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name :: (PersistEntityBackend record ~ SqlBackend, PersistEntity record)
         => EntityField record typ -> Text
    name x =
        (if includeTable
            then ((tn <> ".") <>)
            else id)
        $ connEscapeName conn $ fieldName x

-- | Generates sql for limit and offset for postgres, sqlite and mysql.
decorateSQLWithLimitOffset::Text -> (Int,Int) -> Bool -> Text -> Text
decorateSQLWithLimitOffset nolimit (limit,offset) _ sql =
    let
        lim = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' nolimit
                (_, _) -> " LIMIT " <> T.pack (show limit)
        off = if offset == 0
                    then ""
                    else " OFFSET " <> T.pack (show offset)
    in mconcat
            [ sql
            , lim
            , off
            ]


-- | Mark the subset of 'PersistField's that can be searched by a LIKE query
-- Anything stored as PersistText or PersistByteStrirng
class PersistField typ => SqlLikeSearchable typ where

instance SqlLikeSearchable Text
instance SqlLikeSearchable BS.ByteString
instance SqlLikeSearchable rs => SqlLikeSearchable (Maybe rs)

-- | Filter using LIKE, ILIKE, and its negations
-- Note that a LIKE query such as 'prefix-%' (wildcard suffix only) is able to use standard indexes
-- to match the prefix portion.
(~~.), (~~*.), (!~~.), (!~~*.)
    :: forall record searchable. (SqlLikeSearchable searchable, PersistEntity record, PersistEntityBackend record ~ SqlBackend)
    => EntityField record searchable -> Text -> Filter record
fld ~~. val = BackendFilter $ LikeFilter fld val
fld ~~*. val = BackendFilter $ ILikeFilter fld val
fld !~~. val = BackendFilter $ NotLikeFilter fld val
fld !~~*. val = BackendFilter $ NotILikeFilter fld val

type instance BackendSpecificFilter SqlBackend record = SqlLikeFilter record
data SqlLikeFilter record =
        forall typ. PersistField typ =>
          LikeFilter
            (EntityField record typ)
            Text
      | forall typ. PersistField typ =>
          NotLikeFilter
            (EntityField record typ)
            Text
      | forall typ. PersistField typ =>
          ILikeFilter
            (EntityField record typ)
            Text
      | forall typ. PersistField typ =>
          NotILikeFilter
            (EntityField record typ)
            Text
