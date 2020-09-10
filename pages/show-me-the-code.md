# Show me the code!
This is a whirlwind tour of WebGear. If you want to learn more, go to the [user guide](/guide/introduction).

## Building Request Handlers
A request handler is just a function that takes a request value and produces a response value in a monadic context. This
is where the business logic of your application lives.

```hs
-- The 'Has (JSONRequestBody Widget) r' constrait indicates that this handler
-- can only be called if the request body is a valid JSON that can be parsed
-- to a Widget value.
putWidget :: Has (JSONRequestBody Widget) r => Handler r Widget
putWidget = Kleisli $ \req -> do
  let widget = get (Proxy @(JSONRequestBody Widget)) req
  saveToDB widget
  return (ok200 widget)
```

The `#!hs Handler req Widget` type in the example above is equivalent to the function type
`#!hs Monad m => Linked req Request -> m (Response Widget)` in principle. These type of functions are also known as
Kleisli arrows; hence the use of `#!hs Kleisli` in the handler.

## Accessing Request Traits
Traits are attributes associated with the request. Header values, query parameters, path components, request body are
all examples of traits.

The trait values are accessed using the `#!hs get` function.

```hs
someHandler :: Have [ PathVar "widgetId" UUID
                    , JSONRequestBody Widget
                    , QueryParam "limit" Integer
                    ] r
            => Handler r a
someHandler = Kleisli $ \req -> do
  let
    widget = get (Proxy @(JSONRequestBody Widget)) req     -- widget :: Widget
    wid = get (Proxy @(PathVar "widgetId" UUID)) req       -- wid :: UUID
    limit = get (Proxy @(QueryParam "limit" Integer)) req  -- limit :: Integer
  ...
  return noContent204
```

## Sending Responses
The response value returned contains the HTTP status code, response headers, and optionally a body. The body can be any
type that can be converted to a lazy ByteString using the `#!hs ToByteString` type class.

```hs
createWidget = Kleisli $ \req -> do
  ...
  return (created201 widget)
```

In addition to functions such as `#!hs ok200` and `#!hs created201`, there is a generic `#!hs respond` function that
accepts an HTTP status code, response headers and the body to generate a response.

## Routing without TemplateHaskell
You can use regular functions instead of TemplateHaskell QuasiQuotes for routing if you prefer that.

```hs
-- matches GET /hello/name:String
helloRoute :: Handler '[] String
helloRoute = method @GET
             $ path @"hello" $ pathVar @"name" @String
             $ Kleisli $ \req -> do
                 let name = get (Proxy @(PathVar "name" String)) req
                 return $ ok200 $ "Hello, " ++ name
```

## Composable routes
Compose routes with `#!hs <|>`. During request routing, each of these routes will be tried sequentially and the first
matching handler will be called.

```hs
allRoutes :: Handler r a
allRoutes = createWidget <|> getWidget <|> updateWidget <|> deleteWidget
```

Since handlers are functions, you can refactor and compose them as you wish. The below example shows an application with
v1 and v2 path prefixes.

```hs
allRoutes :: Handler r a
allRoutes = v1Routes <|> v2Routes

-- matches any path starting with /v1
v1Routes :: Handler r a
v1Routes = [match| /v1 ]
           $ v1CreateWidget <|> v1GetWidget <|> v1UpdateWidget <|> v1DeleteWidget

-- matches any path starting with /v2
v2Routes :: Handler r a
v2Routes = [match| /v2 ]
           $ v2CreateWidget <|> v2GetWidget <|> v2UpdateWidget <|> v2DeleteWidget
```

## Middlewares
Middlewares are handlers that wrap another handler. They usually modify or enhance the bahaviour of the inner
handler. For example, the `#!hs queryParam` middleware ensures that the inner handler is invoked only when the request
contains a query parameter named `limit`.

```hs
searchWidget :: Handler '[] a
searchWidget = queryParam @"limit" @Int searchHandler

searchHandler :: Has (QueryParam "limit" Int) r => Handler r a
searchHandler = ...
```

Typically you will compose many middlewares to form a route handler. The [user guide](/guide/middlewares) explains how you can
build your own middlewares.

## First-class JSON support
Use JSON input and output in your handlers without any boilerplate. WebGear uses the
[aeson](https://hackage.haskell.org/package/aeson) library to handle JSON values. The request body can be read as a JSON
value using `#!hs jsonRequestBody` middleware. The response can be converted to a lazy ByteString using the `#!hs
jsonResponseBody` middleware.

```hs
updateWidget :: Handler '[] ByteString
updateWidget = jsonRequestBody @Widget
               $ jsonResponseBody @Widget
               $ updateWidgetHandler

updateWidgetHandler :: Has (JSONRequestBody Widget) r => Handler r Widget
updateWidgetHandler = Kleisli $ \req -> do
  let widget = get (Proxy @(JSONRequestBody Widget)) req  -- widget :: Widget
  ...
  return $ ok200 widget
```
