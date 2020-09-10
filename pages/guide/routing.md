# Routing
The applications we built so far had a single handler. However, most web applications will have many handlers each one
for serving different HTTP methods, URL paths etc. Let us see how to achieve this in WebGear.

## Alternatives
We know that handlers are kleisli arrows. This means that we can combine multiple handlers using the `#!hs <|>` operator
from `#!hs Alternative` type class. Thus if we have two handlers `#!hs getTime` and `#!hs setTime`, we could create a
combined handler like this.

```hs
timeHandler = getTime <|> setTime
```

So, how do we decide which handler gets invoked for a given request? How do we "route" a request to a specific handler?
It turns out we can do this using a middleware.

## Router Monad
As explained in the [traits](../traits) section, handler are kleisli arrows on the `#!hs Router` monad. However, it is
useful to look at the `#!hs MonadRouter` type class instead of using this monad directly.

```hs
class (Alternative m, MonadPlus m) => MonadRouter m where
  -- | Mark the current route as rejected, alternatives can be tried
  rejectRoute :: m a

  -- | Short-circuit the current handler and return a response
  errorResponse :: Response ByteString -> m a

  -- | Handle an error response
  catchErrorResponse :: m a -> (Response ByteString -> m a) -> m a
```

The `#!hs rejectRoute` method can be used by middlewares and handlers to flag the current route as "not matching", so
that the next route is tried. This forms the basis of routing. For example, given this code:

```hs
-- Reject the route so that alternatives will be tried
method :: MonadRouter m => Middleware' m req (Method t:req) a a
method handler = Kleisli $ \request -> do
  res <- probe @(Method t) request
  either (const rejectRoute) (runKleisli handler) res

timeHandler = getTime <|> setTime

getTime :: Handler req a
getTime = method @GET getTimeHandler

setTime :: Handler req a
setTime = method @POST setTimeHandler
```

The `#!MonadRouter` will try each route sequentially. If any of them calls `#!hs rejectRoute`, the next one will be
tried and the response from the first matching handler will be returned. If none of the route handlers matched, a `404
Not Found` response will be returned.

The `#!hs errorResponse` and `#!hs catchErrorResponse` are used in cases where you want to indicate that the current
handler matches the route but you want to return an exceptional response. This is similar to the exception handling
mechanisms offered by the `#!hs MonadError` type class.

## Middlewares for Routing
As you can see from the above description, the routing mechanism is very flexible in WebGear. Virtually any middleware
or handler can invoke `#!hs rejectRoute` to skip the current handler and try the next. WebGear does not assume anything
about which request attributes are used in the handler selection. That decision is left to middlewares and handlers.

However, for most usecases, you want to route based on the HTTP method and/or the URL path. WebGear provides a number of
middlewares that support this common usecase.

- `#!hs method` attempts to match an HTTP method.
- `#!hs path` attempts to match a prefix portion of the request URL path.
- `#!hs pathVar` attempts to parse the next component from the URL path to a value via `#!hs FromHttpApiData` type
  class.
- `#!hs pathEnd` succeeds only if all URL path components are already consumed by `#!hs path` or `#!hs pathVar`

Here is how you would use them:

```hs
-- Matches a GET request on URL /v1/widgets/<widgetId>
-- where <widgetId> can be parsed as an Int
getWidget :: Handler req a
getWidget = method @GET
            $ path @"/v1/widgets"
            $ pathVar @"widgetId" @Int
            $ pathEnd
            $ getWidgetHandler
```

If you prefer a less verbose version, you can use template haskell quasiquoter:

```hs
getWidget :: Handler req a
getWidget = [route| GET /v1/widgets/widgetId:Int |] getWidgetHandler
```
