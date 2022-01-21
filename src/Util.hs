module Util where


showEither :: Show a => Show b => Either a b -> String
showEither (Left a) = show a
showEither (Right a) = show a

