{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

import Network.HTTP.Types (StdMethod (GET))
import Network.Wai.Handler.Warp
import WebGear

routes :: Handler '[] String
routes = [route| GET /hello/name:String/ |] $ Kleisli $
           \request -> do
             let Tagged name = get @(PathVar "name" String) request
             return $ ok200 $ "Hello, " ++ name

main :: IO ()
main = run 3000 (toApplication routes)
