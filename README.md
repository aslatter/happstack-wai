This package provides a simple adapter to run a [Happstack][1]
application on a [WAI][2] host, such as [Warp][3].

My goal has been to explore what it would take for Happstack
to addopt a WAI backend as standard, instead of implementing
its own HTTP server code.

As such, I take some shortcuts in the implementation where
it doesn't matter for testing - currently we only send HTTP
response codes without response descriptions, and we do not
respond well to a unknown HTTP method.

[1]: http://happstack.com
[2]: http://hackage.haskell.org/package/wai
[3]: http://hackage.haskell.org/package/warp
