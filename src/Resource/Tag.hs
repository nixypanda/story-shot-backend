{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Resource.Tag
  ( getTagResources
  , getTagResource
  , createTagResources
  , createTagResource
  , updateTagResources
  , updateTagResource
  , deleteTagResources
  , deleteTagResource
  ) where

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Tag as TT
import qualified Type.Meta as TM
import qualified Storage.Tag as ST


-- CREATE

createTagResource :: TT.TagInsert -> I.WithConfig (TD.Document TT.Tag)
createTagResource = fmap TM.indexDocument' . ST.createTag


createTagResources :: [TT.TagInsert] -> I.WithConfig (TD.Document TT.Tag)
createTagResources = fmap TM.docMulti . ST.createTags



-- RETRIVE

getTagResources :: TP.CursorParam -> I.WithConfig (TD.Document TT.Tag)
getTagResources cp = TM.docMulti <$> ST.getTags cp


getTagResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
getTagResource = fmap TM.docOrError . ST.getTag



-- UPDATE

updateTagResource :: TT.TagPut -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
updateTagResource = fmap TM.docOrError . ST.updateTag


updateTagResources :: [TT.TagPut] -> I.WithConfig (TD.Document TT.Tag)
updateTagResources = fmap TM.docMulti . ST.updateTags



-- DELETE

deleteTagResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
deleteTagResource = fmap TM.docMetaOrError . ST.deleteTag


deleteTagResources :: [Int] -> I.WithConfig (TD.Document TT.Tag)
deleteTagResources = fmap (TM.docMeta . fromIntegral) . ST.deleteTags
