# First Application
Let us build our first WebGear application. We'll start off with a simple one, the goal of our first application is to
respond with the current time for every HTTP request.

Modify `src/Main.hs` to contain the following code.

```hs
import Control.Monad.IO.Class
import Data.Time.Clock
import Network.Wai.Handler.Warp
import WebGear

getTime :: MonadIO m => Kleisli m a (Response String)
getTime = Kleisli $
  \request -> do
    t <- liftIO getCurrentTime
    return $ ok200 $ show t

main :: IO ()
main = run 3000 (toApplication getTime)
```

Build and run the project:

=== "Stack"
    ```shell
    stack build
    stack exec webgear-guide
    ```
=== "Cabal"
    ```shell
    cabal run webgear-guide
    ```

Access <http://localhost:3000> and the application should return the current UTC time.

Let us check the code in detail to understand this clearly.

## WAI and Warp
We will first examine `#!hs main` function.

Like many other Haskell web frameworks, WebGear uses [WAI](https://hackage.haskell.org/package/wai) and
[Warp](https://hackage.haskell.org/package/warp). WAI provides an interface for web applications to interact with web
servers. Warp is a web server based on WAI.

In the `#!hs main` function, `#!hs toApplication` is used to convert our WebGear handler to a WAI application. This is
then passed to `#!hs run` which starts a Warp server serving HTTP requests arriving at port 3000.

## Handlers
Now we turn our attention to `#!hs getTime`. This is where the business logic of our application lives. In WebGear
terminology this is called a Handler. There are a number of interesting things going on here.

First, this is a function wrapped in `#!hs Kleisli` from
[Control.Arrow](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html#t:Kleisli) module. These are
called Kleisli arrows which is just a fancy term for functions having the type `#!hs Monad m => a -> m b`. So why don't
we use just a regular function instead of this `#!hs Kleisli` wrapper? Well, it gives us some useful type class
instances such as `#!hs Alternative` and `#!hs MonadPlus` which will come in handy later. We'll see this when we learn
about routing.

The function inside `#!hs Kleisli` takes a request value as its parameter and produces a monadic response object as its
value. We don't make use of the request in this function because we always return the current time. We also don't care
about which monad this runs under as long as it is a `#!hs MonadIO`.

Finally, the response is produced with `#!hs ok200 $ show t`. As you would have guessed, this generates an HTTP `200 OK`
response with the body produced by `#!hs show t`. We return a `#!hs String` body in this case, but it can be any type
with a
[ToByteString](https://hackage.haskell.org/package/bytestring-conversion-0.3.1/docs/Data-ByteString-Conversion-To.html#t:ToByteString)
instance.
