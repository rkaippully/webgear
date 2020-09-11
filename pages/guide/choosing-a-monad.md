# Choosing a Monad
So far, all our handlers used the `#!hs Handler` type which in turn used the `#!hs Router` monad. But this won't work if
you want to use your own monad transformer stack or an algebraic effect system. How do we solve that?

## Type of Handlers
The handler and middleware types are defined as:

```hs
type Handler' m req a = Kleisli m (Linked req Request) (Response a)

type Handler req a = Handler' Router req a

type Middleware' m req req' a' a = Handler' m req' a' -> Handler' m req a

type Middleware req req' a' a = Middleware' Router req req' a' a
```

You can use any monad of your choice `#!hs m` with a `#!hs Handler'` or `#!hs Middleware'`. However, `#!hs m` must be an
instance of `#!hs MonadRouter` to make use of the routing features. You could build such an instance for your
monad. However, it is a lot easier if you can find a way to transform monadic values in `#!hs m` to `#!hs Router`.

## Transformations
There is a function for that!

```hs
transform :: (forall x. m x -> Router x)
          -> (Handler' m req a -> Handler' Router req a)
transform = ...
```

You provide a function that can translate values in your monad `#!hs m` to values in `#!hs Router`. And now you can
translate handlers in your custom monad to handlers in `#!hs Router` monad. Isn't that handy?

For example, if you want to use the `#!hs getWidget` route from the previous section with your own monad stack, this is
what you would do:

```hs
getWidget :: Handler' Router req a
getWidget = method @GET
            $ path @"/v1/widgets"
            $ pathVar @"widgetId" @Int
            $ pathEnd
            $ handler getWidgetHandler

handler :: Handler' (ReaderT env IO) req a -> Handler' Router req a
handler = transform readerToRouter

readerToRouter :: ReaderT env IO a -> Router a
readerToRouter = liftIO . flip runReaderT env
```
