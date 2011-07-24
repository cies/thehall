{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Prelude hiding (catch)
import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
{- import           Snap.Extension.Timer-}
import           Snap.Util.FileServe
import           Snap.Types
import           Data.Maybe (fromMaybe, fromJust, isJust, isNothing, mapMaybe)

import           Application

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Word  -- a Word is an unsigned Int

import           Control.Exception --(evaluate)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX -- (posixSecondsToUTCTime)
import           Data.Time.Clock (getCurrentTime)

import           Models.Post


postsGet :: Application ()
postsGet = do
  since <- getParam "since"
  -- case since of Just s -> writeBS s
  case since of
    Just str -> do
      n <- liftIO $ catch (evaluate (read $ B8.unpack str :: Word))
                          (\e -> do let _ = e :: ErrorCall
                                    evaluate 0)
      -- writeBS $ B8.pack ("//" ++ (show n) ++ "//")
      case n > 1 of
        True  -> continueWith $
          Just $ posixSecondsToUTCTime
            (fromInteger (fromIntegral n :: Integer) :: POSIXTime)
        False -> continueWith Nothing
    Nothing -> continueWith Nothing
  where
    {- setResponse x = (writeBS . B8.pack . show) =<< getPosts x-}
    {- setResponse x = (writeBS . B8.pack . encodeJSON) =<< getPosts x-}
    continueWith x = (writeBS . postsToJSON) =<< getPosts x

postsPost :: Application ()
postsPost = do
  g <- getParam "body"
  h <- getParam "author"
  i <- getParam "avatarPath"
  case (g,h,i) of
       (Just body, Just author, Just avatarPath) -> do
         now <- liftIO $ getCurrentTime
         let post = Post Nothing now body author avatarPath
         oid <- savePost post
         modifyResponse $ setResponseStatus 201 "Post created"
         writeBS $ BS.concat ["{\"id\":\"", oid, "\"}"]
       _ -> modifyResponse $ setResponseStatus 400 "Malformed parameters"

postsDelete :: Application ()
postsDelete = writeText "Not implemented"

site :: Application ()
site = route [ ("/api/posts",     method GET    $ postsGet)
             , ("/api/posts",     method POST   $ postsPost)
             , ("/api/post_post", method GET    $ postsPost)  -- convenience
             , ("/api/posts/:id", method DELETE $ postsDelete)
             , ("/api/post_del/:id", method GET    $ postsDelete)
             ]
       <|> serveDirectory "resources/static"





-- // Environment, constants, etc
-- var MAXSIZE = 25 // # of posts per request

-- var posts = db.collection('posts');

-- // Express setup
-- var app = express.createServer();
-- app.use(express.bodyParser());
-- app.use(app.router);
--
-- // GET /api/posts
-- app.get('/api/posts', function(req, res){
--   var _posts; // result Array
--
--   if (req.query.since && req.query.since > 1) {
--     posts.find({time:{$gt:req.query.since}}).sort({time:-1}).limit(MAXSIZE).toArray(function(err, _posts){
--       res.send(_posts.reverse()); // so latest posts come last
--     });
--   }
--   else {
--     posts.find().sort({time:-1}).limit(MAXSIZE).toArray(function(err, _posts){
--       res.send(_posts.reverse()); // so latest posts come last
--     });
--   }
-- });
--
-- // POST /api/posts
-- app.post('/api/posts', function(req, res){
--   var entry = req.body;
--   entry.time = (new Date()).getTime().toString();
--
--   // db: save
--   posts.save(entry, function(err, finalEntry){
--     var _id = finalEntry._id.toString();
--     res.send({id:_id}, 201); // 201 == Created
--   });
-- });
--
-- // DELETE /api/posts/:id
-- app.del('/api/posts/:id', function(req, res){
--   posts.remove({_id: ObjectId(req.params.id)});
--   // response
--   res.send({}, 200);
-- });
--
-- app.listen(PORT);
--
-- console.log('Listening on port '+PORT);PORT
