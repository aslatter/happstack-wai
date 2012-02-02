Here I'll go over steps that can be taken to smooth the boundary
between Happstack an WAI.

I'll divide up the changes into those that have the least impact
on the happstack-server back-end, and those that would require
whole-scale adoption of WAI (or a substantial re-write of the
existing server code).

### Compatibility

* Adopt the 'HttpVersion' type from the 'http-types'
package.
* Adopt the 'Method' type from the 'http-types' package.
This would require some thought into what API we would
present to users of the happstack-server library, since
the typea aren't exactly one-to-one.
* Use ByteStrings for the raw path and raw query string.
* Provide the 'path segments' as Data.Text instead of String.
I'm not really sure how much this matters. Other
happstack-related libraries (web-routes) are moving in this
direction.
* Provide a version of 'mkHeaders' which works with
bytestrings instead of strings.

### Adopting WAI

If we decide to use WAI as *the* back-end for
happstack we can thread through the 'conduit'
approach to IO to users of happstack-server.

* Drop the response flags - I don't know how to
translate them to WAI. The approach to
comet-on-happstack would have to change, though
for application developers.
* Store the request body as a 'Source IO ByteString',
allowing handlers to use conduit-based libraries to
process the request-body without lazy IO.
* Modify the response to return a 'Source IO Builder'
in addition to the other current options. I guess we
could convert it to an LBS if we're not on the WAI
back-end, but that feels a bit sketchy to me.
* I don't think it's worth it adopting the WAI response
type directly - I guess the main thing we would lose
would be that the headers would be a list instead of
a map. Maybe it is worth it.
