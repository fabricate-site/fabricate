# Fabricate
**Form by art and labor**

## Introduction 
Fabricate is a Clojure library for making static websites, using Clojure.

Fabricate gives you something many other tools don't: the power to evaluate Clojure code to generate the contents of a page. The “using Clojure” part of that first sentence contains the core idea: Fabricate is built on the idea that you should be able to use Clojure to generate as much (or little) of your website as is necessary.

Please see [fabricate.site](https://fabricate.site) for more documentation.
## Core API

Fabricate also gives you an API that can generate a website using 3 core functions.

```clojure
(require '[site.fabricate.api :as api])

(->> {}  
    (api/plan! [])
    (api/assemble  [])
    (api/construct! [])
```

This API, while straightforward enough to quickly get started with, does not constrain what you can do with Fabricate. You can extend Fabricate to new markup formats or other methods of generating pages. If you use Clerk for some things and markdown for others, Fabricate can flexibly accommodate all of them with a unified API.

It does this by defining 3 multimethods, each one corresponding to each of the 3 functions above:
1. sources (`api/collect`)
2. building documents from sources (`api/build`)
3. generating pages from documents (`api/produce!`)

The [API documentation](https://fabricate.site/reference/namespaces/site.fabricate.api.html) provides a more detailed description of how these all fit together.

## Sources
Fabricate includes functions to generate page data from two sources:
- Templates containing Clojure expressions
- Clojure files

Markdown support is planned, as is greater compatibility with the emerging [kindly](https://github.com/scicloj/kindly) protocol for flexible display of Clojure values.

## Status

Fabricate's core API – `site.fabricate.api` is now stable. Any namespace containing the `prototype` prefix should be considered subject to change. Namespaces will be "promoted" from prototype to stable as I refine them.