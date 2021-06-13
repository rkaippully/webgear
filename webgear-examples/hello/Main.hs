{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

import Network.HTTP.Types (StdMethod (GET))
import Network.Wai.Handler.Warp (run)
import WebGear

routes :: Handler '[] String
routes = [route| GET /hello/name:String/ |] $
  Kleisli $ \request -> do
    let name = pick @(PathVar "name" String) $ from request
    return $ ok200 $ "Hello, " ++ name

main :: IO ()
main = run 3000 (toApplication routes)
