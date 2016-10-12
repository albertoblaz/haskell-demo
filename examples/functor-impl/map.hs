import qualified Data.Map as Map
import qualified Data.List as List

class FFunctor f where ffmap :: (a -> b) -> f a -> f b

instance (Ord k) => FFunctor (Map.Map k) where
  ffmap f = Map.fromList . map (\(p,q) -> (p, f q)) . Map.toList

type Name = String
type Names = [Name]

fillMap :: (Ord k, Enum k, Num k) => [v] -> (Map.Map k v)
fillMap names = foldr (\(p,q) m -> Map.insert p q m) Map.empty $ zip [0..] names

showValues :: (Ord k, Show v) => (Map.Map k v) -> [(k, String)]
showValues map = Map.toList $ ffmap show map

-- let names = ["Alberto", "Adrian", "Martin", "Brent"]
