This package provides a simple adapter to run a [Happstack][1]
application on a [WAI][2] host, such as [Warp][3].

My goal is to explore what it would take for Happstack
to addopt a WAI backend as standard, instead of implementing
its own HTTP server code. I'm interested in what the interface
looks like - where it is smooth and where it is cumbersome.

As such, I take some shortcuts in the implementation where
it doesn't matter.

[1]: http://happstack.com
[2]: http://hackage.haskell.org/package/wai
[3]: http://hackage.haskell.org/package/warp
