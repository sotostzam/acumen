import Text.JSON
import Text.JSON.String
import Control.Arrow((***))
import Control.Applicative
import Data.Ratio

type Name = String

data StepType = Discrete | Continuous
  deriving Show

data Value = Int Int           
           | Double Double     
           | Bool Bool      
           | Str String        
           | List [Value]  
           | Vector [Value]
           | ObjId Int      
           | ClassName Name
           | StepType StepType 
  deriving Show

newtype Obj = Obj [(Name,Value)]
  deriving Show

newtype Store = Store [(Int,Obj)]
  deriving Show

value ty v = makeObj [("type", JSString $ toJSString ty), ("value", showJSON v)]

instance JSON StepType where
  showJSON Discrete   = JSString $ toJSString "Discrete"
  showJSON Continuous = JSString $ toJSString "Continuous"

  readJSON (JSString ss) = 
    case fromJSString ss of
      "Discrete"   -> Ok Discrete
      "Continuous" -> Ok Continuous
      _            -> Error "expecting step type"

instance JSON Value where
  showJSON (Int i)       = value "int" $ JSRational False (toRational i)
  showJSON (Double d)    = value "double" $ JSRational False (toRational d)
  showJSON (Bool b)      = value "boolean" $ JSBool b
  showJSON (Str s)       = value "string" $ toJSString s
  showJSON (List l)      = value "list" . JSArray $ map showJSON l
  showJSON (Vector l)    = value "vector" . JSArray $ map showJSON l
  showJSON (ObjId i)     = value "objId" $ JSRational False (toRational i)
  showJSON (ClassName n) = value "className" $ toJSString n
  showJSON (StepType s)  = value "stepType" $ showJSON s

  readJSON (JSObject o) = do
    ty  <- readJSON =<< valFromObj "type" o
    key <- valFromObj "value" o
    convert ty key
    where convert "int" (JSRational False r) = Int <$> extractInt r
          convert "double" (JSRational False r) = return . Double $ fromRational r
          convert "boolean" (JSString ss) = return . Bool $ 
            case fromJSString ss of 
              "true" -> True 
              "false" -> False 
          convert "string" (JSString ss) = return . Str $ fromJSString ss
          convert "list" (JSArray a) = List <$> mapM readJSON a
          convert "convert" (JSArray a) = Vector <$> mapM readJSON a
          convert "objId" (JSRational False r) = ObjId <$> extractInt r
          convert "className" (JSString ss) = return . ClassName $ fromJSString ss
          convert "stepType" st = StepType <$> readJSON st
          convert _ _ = Error $ "expected a value, got " ++ show o
    
          extractInt :: Rational -> Result Int
          extractInt r | denominator r == 1 = return . fromInteger $ numerator r
                       | otherwise          = Error "not an integer"
 
  readJSON o = Error $ "expected a value, got " ++ show o

instance JSON Obj where
  showJSON (Obj e) = makeObj $ map (id *** showJSON) e

  readJSON (JSObject o) = Obj <$> mapM dec (fromJSObject o)
    where dec (i,o) = do v <- readJSON o; return (i,v)
  readJSON _ = Error "expecting fields"

instance JSON Store where
  showJSON (Store s) = makeObj $ map (show *** showJSON) s

  readJSON (JSObject o) = Store <$> mapM dec (fromJSObject o)
    where dec (k,v) = do o <- readJSON v; return (read k, o)
  readJSON _ = Error "expecting store"

parseStore :: String -> Either String Store
parseStore = resultToEither . decode

main = getContents >>= print . parseStore
