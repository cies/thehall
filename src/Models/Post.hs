{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Models.Post where

import            Snap.Extension
import qualified  Snap.Extension.DB.MongoDB as DB
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)
import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Typeable
import            Data.Maybe (catMaybes, listToMaybe, isNothing)
import            Data.List (intersperse)
import            Data.Word
import            Data.Time.Clock
import            Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader
import            Application

import           Numeric (showHex, readHex)

{- import           Text.JSON.Generic (encodeJSON)-}

-- { "msg":"bing", "avatarPath":"img/avatar_frankenstein_halloween_monster_icon.png",
--   "name":"yarr", "time":"1311410914962", "_id":"4e2a8ae2d3cacc01000000b2" }
data Post = Post { pId         :: Maybe ObjectId
                 , pCreatedAt  :: UTCTime
                 , pBody       :: BS.ByteString
                 , pAuthor     :: BS.ByteString
                 , pAvatarPath :: BS.ByteString }
                 deriving (Show, Eq, Typeable, Ord)

instance Val Post where
    val (Post id' createdAt body author avatarPath) =
      Doc [ "_id" =: id', "createdAt" =: createdAt, "body" =: body,
            "author" =: author, "avatarPath" =: avatarPath ]
    cast' (Doc fields) = do
      id'        <- B.lookup "_id"        fields
      createdAt  <- B.lookup "createdAt"  fields
      body       <- B.lookup "body"       fields
      author     <- B.lookup "author"     fields
      avatarPath <- B.lookup "avatarPath" fields
      return (Post id' createdAt body author avatarPath)
    cast' _ = Nothing

objid2bs' :: ObjectId -> BS.ByteString
objid2bs' (Oid a b) = B8.pack . showHex a . showHex b $ ""

postToJSON :: Post -> BS.ByteString
postToJSON p = BS.concat [
    B8.pack "{",
    (case pId p of Just x -> BS.concat [B8.pack "\"_id\":\"", objid2bs' x, B8.pack "\", "]
                   Nothing -> B8.pack ""),
    B8.pack "\"createdAt\":\"",
      B8.pack $ show $ truncate (utcTimeToPOSIXSeconds $ pCreatedAt p),
      B8.pack "\", ",
    B8.pack $ "\"body\":\"", pBody p, B8.pack "\", ",
    B8.pack $ "\"author\":\"", pAuthor p, B8.pack "\", ",
    B8.pack $ "\"avatarPath\":\"", pAvatarPath p, B8.pack "\"}"
  ]

postsToJSON :: [Post] -> BS.ByteString
postsToJSON ps = BS.concat $ [ B8.pack "[",
                               BS.concat $ intersperse (B8.pack ", ") (map postToJSON ps),
                               B8.pack "]" ]

emptyPost = do
  now <- getCurrentTime
  return $ val $ Post Nothing now "1" "2" "3"

maxSize = 25

getPosts :: Maybe UTCTime -> Application [Post]
getPosts since = do
  c <- DB.withDB $ DB.find $
   ( DB.select ( case since of Just ts -> [ "createdAt" =: ["$gt" =: ts] ]
                               _       -> [] )  "posts" )
   { DB.limit  = maxSize
   , DB.sort   = ["createdAt" =: (-1 :: Int)] }
  case c of
    Left  _  -> return []  -- error
    Right cs -> do
      docs <- DB.withDB $ DB.rest cs
      case docs of
        Left  _  -> return []  -- error
        Right ps -> return $ catMaybes $ map (cast' . Doc) ps

savePost :: Post -> Application BS.ByteString
savePost post = do
  c <- DB.withDB $ DB.insert "posts" (processNew $ unDoc $ val post)
  case c of Left _ -> return undefined
            Right x -> return $ objid2bs' $ typed x
  where unDoc (Doc fields) = fields
        processNew fields  = if   isNothing (B.lookup "_id" fields :: Maybe ObjectId)
                             then exclude ["_id"] fields
                             else fields

deletePost :: ObjectId -> Application ()
deletePost oid = do
  DB.withDB $ DB.delete $ DB.select [ "_id" =: oid ] "posts"
  return ()

