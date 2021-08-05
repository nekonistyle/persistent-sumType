{-# LANGUAGE TemplateHaskell #-}

module MyPersistField where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Sql

import Data.Text as T


-- Either

instance (PersistField a, PersistField b) => PersistField (Either a b) where
  toPersistValue (Left a) = PersistList [PersistBool True, toPersistValue a]
  toPersistValue (Right b) = PersistList [PersistBool False, toPersistValue b]
  fromPersistValue v =
    case fromPersistValue v of
      Right [PersistBool True,va] -> Left <$> fromPersistValue va
      Right [PersistBool False,vb] -> Right <$> fromPersistValue vb
      Left e -> Left e
      _ -> Left $ T.pack $ "Expected 2 item PersistList, received: " ++ show v

instance (PersistFieldSql a, PersistFieldSql b) => PersistFieldSql (Either a b) where
  sqlType _ = SqlString



-- MySumType

data MySumType a b1 b2
  = Constructor1
  | Constructor2 a
  | Constructor3 b1 b2
  deriving Show

instance (PersistField a, PersistField b1, PersistField b2) => PersistField (MySumType a b1 b2) where
  toPersistValue Constructor1 =
    PersistList [PersistInt64 1]
  toPersistValue (Constructor2 a) =
    PersistList [PersistInt64 2, toPersistValue a]
  toPersistValue (Constructor3 b1 b2) =
    PersistList [PersistInt64 3, toPersistValue b1, toPersistValue b2]
  fromPersistValue v =
    case fromPersistValue v of
      Right [PersistInt64 1] -> Right Constructor1
      Right [PersistInt64 2,va] -> Constructor2 <$> fromPersistValue va
      Right [PersistInt64 3,vb1,vb2] ->
        Constructor3 <$> fromPersistValue vb1 <*> fromPersistValue vb2
      Left e -> Left e
      _ -> Left $ T.pack $ "Expected PersistList [1], [2,_] or [3,_,_], received: " ++ show v

instance (PersistFieldSql a, PersistFieldSql b1, PersistFieldSql b2) => PersistFieldSql (MySumType a b1 b2) where
  sqlType _ = SqlString




