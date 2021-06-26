# fabricate

Static website generation beyond the limitations of markdown.

## Usage

Evaluate and publish the pages in the docs folder:

    $ clojure -X site.fabricate.prototype.write/publish '["./docs"]'

Set up a live-reloading environment in which pages are republished as they get saved:

    $ clojure -X site.fabricate.prototype.write/draft

Run the project's tests:

    $ clojure -M:test:runner

## License

Distributed under the Eclipse Public License version 2.0.
