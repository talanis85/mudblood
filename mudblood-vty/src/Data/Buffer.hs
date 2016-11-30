module Data.Buffer where

import qualified Data.PrefixZipper as Z

data Buffer = Buffer
    { bufferContent :: String
    , bufferCursor :: Int
    , bufferCompletionList :: Z.PrefixZipper Char
    , bufferCompletionState :: Bool
    }

bufferEmpty :: Buffer
bufferEmpty = Buffer
    { bufferContent = ""
    , bufferCursor = 0
    , bufferCompletionList = Z.empty
    , bufferCompletionState = False
    }

bufferModifyCursor :: (Int -> Int) -> Buffer -> Buffer
bufferModifyCursor f buf = buf
    { bufferCursor = f (bufferCursor buf) }

bufferSetContent :: String -> Buffer -> Buffer
bufferSetContent str buf = bufferUpdateCompletion $ buf
    { bufferContent = str
    , bufferCursor = min (length str) (bufferCursor buf)
    , bufferCompletionState = False
    }

bufferInsert :: Char -> Buffer -> Buffer
bufferInsert char buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take cursor content ++ [char] ++ drop cursor content
        , bufferCursor = cursor + 1
        }

bufferDeleteLeft :: Buffer -> Buffer
bufferDeleteLeft buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take (cursor - 1) content ++ drop cursor content
        , bufferCursor = max 0 (cursor - 1)
        }

bufferDeleteRight :: Buffer -> Buffer
bufferDeleteRight buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take cursor content ++ drop (cursor + 1) content
        , bufferCursor = min (length content) (cursor + 1)
        }

bufferUpdateCompletion :: Buffer -> Buffer
bufferUpdateCompletion buf = bufferModifyCompletion (Z.setPrefix (bufferContent buf)) buf

bufferUpdateContent :: Buffer -> Buffer
bufferUpdateContent buf = case Z.cursor (bufferCompletionList buf) of
    Nothing -> buf
    Just c  -> buf { bufferContent = c, bufferCursor = (length c) }

bufferSetCompletion :: [String] -> Buffer -> Buffer
bufferSetCompletion comp buf = bufferUpdateCompletion $ buf
    { bufferCompletionList = Z.fromList comp
    , bufferCompletionState = False
    }

bufferSetCompletionState :: Bool -> Buffer -> Buffer
bufferSetCompletionState v buf = buf { bufferCompletionState = v }

bufferModifyCompletion :: (Z.PrefixZipper Char -> Z.PrefixZipper Char) -> Buffer -> Buffer
bufferModifyCompletion f buf = buf { bufferCompletionList = f (bufferCompletionList buf) }
