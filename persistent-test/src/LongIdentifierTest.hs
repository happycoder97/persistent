{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module LongIdentifierTest where

import Database.Persist.TH
import Init

-- This test is designed to create very long identifier names

-- MySQL: This test is disabled for MySQL because MySQL requires you to truncate long identifiers yourself. Good easy issue to fix 
-- Postgres automatically truncates too long identifiers to a combination of:
-- truncatedTableName + "_" + truncatedColumnName + "_fkey"
-- 

share [mkPersist sqlSettings, mkMigrate "longIdentifierMigrate", mkDeleteCascade sqlSettings] [persistLowerCase|
TableAnExtremelyFantasticallySuperLongNameParent
    field1 Int
TableAnExtremelyFantasticallySuperLongNameChild
    columnAnExtremelyFantasticallySuperLongNameParentId TableAnExtremelyFantasticallySuperLongNameParentId
|]

specsWith :: (MonadIO m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "Migration" $ do
    it "is idempotent" $ runDb $ do
      again <- getMigration longIdentifierMigrate
      liftIO $ again @?= []
    it "really is idempotent" $ runDb $ do
      runMigration longIdentifierMigrate
      again <- getMigration longIdentifierMigrate
      liftIO $ again @?= []
