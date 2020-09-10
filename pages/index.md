# Welcome to WebGear
WebGear makes it easy to build composable, extensible, type-Safe HTTP APIs in Haskell.

[:fontawesome-solid-file-code: Show me the code!](/show-me-the-code){: .md-button .md-button--primary }
[:fontawesome-solid-book: User Guide](/guide/introduction){: .md-button }

-------------------------------------

## Hello WebGear
Here is a fully functional WebGear application. If you access <http://localhost:3000/hello/Legolas>, you'd get a `200
OK` response with body: `Hello, Legolas`.

```hs
{-# LANGUAGE DataKinds, QuasiQuotes, TypeApplications #-}

import Network.HTTP.Types (StdMethod (GET))
import Network.Wai.Handler.Warp
import WebGear

routes :: Handler '[] String
routes = [route| GET /hello/name:String |] $ Kleisli $
           \request -> do
             let name = get (Proxy @(PathVar "name" String)) request
             return $ ok200 $ "Hello, " ++ name

main :: IO ()
main = run 3000 (toApplication routes)
```

## Developer Friendly
WebGear is built on a small number of simple concepts which makes it approachable. You don't need to understand a
gazillion advanced language features to build type safe APIs. Friendly error messages will guide you when you make
mistakes.

<script id="asciicast-357600" src="https://asciinema.org/a/357600.js" async></script>

## Composability
APIs are built by composing functions to form handlers and middlewares. Build complex APIs from smaller simpler parts!

```hs
putUser = method @PUT
          $ requestContentTypeHeader @"application/json"
          $ jsonRequestBody @User
          $ jsonResponseBody @User
          $ putUserHandler
```

## Extensibility
Every component of WebGear can be replaced with alternative implementations should you wish so. Your custom middlewares
are as first-class as the ones provided by WebGear.

APIs run with the framework of your choice. WebGear can work with monad transformers or algebraic effect systems that
you want to use.

-------------------------------------

Continue to learn more about WebGear.

[:fontawesome-solid-file-code: Show me the code!](/show-me-the-code){: .md-button .md-button--primary }
[:fontawesome-solid-book: User Guide](/guide/introduction){: .md-button }
