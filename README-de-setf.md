Notes onto CL-XML
=================

## Documentation Notes

**CL-XML:** Presently, [de.setf.xml][cl-xml]

## API Notes

### URI Object Model

[CL-XML][cl-xml] may define a URI object model, when the file
`de.setf.xml/code/base/utils.lisp` is evaluated. However, if
[CL-HTTP][cl-http] is loaded as would be assumed, when the feature
`:CL-HTTP` would be defined to the environment's `*features*` list --
when that file is interpreted, or when the [CL-XML][cl-xml] system
definition is interpreted -- then [CL-XML][cl-xml] will use the URI
object model provided by [CL-HTTP][cl-http].

See also:
* `xml-utils::|rfc1738|`

### XML Proccessing

The generic function `XMLP:DOCUMENT-PARSER` is defined by CL-XML. This
function serves as a generic interface for processing of XML
documents, regardless of whether contained in a stream, or in a
resource denoted by a URI, or within a string or other vector.

**Caveats**
* Method `XMLP:DOCUMENT-PARSER (STRING &REST T)`
    * A _system-supplied primary method_
    * If `STRING` begins with the character `#\<` at position 0 in the
      string, then the string will be interpreted as it containing XML
      markup.
    * Otherwise, the string will be interpreted as a URI.
* Method `XMLP:DOCUMENT-PARSER (STREAM &REST T &KEY ... &ALLOW-OTHER-KEYS ...)`
    * This is the "Workhorse method" specialized onto `XMLP:DOCUMENT-PARSER`


* _**Notes**_
    * Documentation about the [CL-XML][cl-xml] native XML processing
      API may be produced automatically. If the following forms
      would be _advised_ when [CL-XML][cl-xml] is _compiled_, it would
      serve to to allow for an automatic listing of _generic
      functions_ applied within the [CL-XML][cl-xml] native XML processing
      API:
        * one of:
            * `XML-UTILS::MAKE-CONSTRUCTOR-NAME`, or...
            * all of (external symbols in `xml-utils`)
                * `defAlternativeConstructor`
                * `defConstantConstructor`
                * `defConstructor`
                * `defConstructorMethod`
                * `defIdentityConstructor`
                * `defLiteralConstructor`
                * `defNullConstructor`
                * `defTokenConstructor`
                * `defTokenConstructors`
        * and (external symbols in `xml-utils`)
            * `defTokens`
    * Considering application of the [ATN-Parser][atn-parser] system
      within [CL-XML][cl-xml], as specifically for XML document
      processing, refer to `"xml:code;xparser;"` --
      specifically `"xml:code;xparser;grammar;xml-grammar.lisp"` -- as
      well as `"xml:bnf;xml-grammar.bnf"`

## Packages and Namespaces

* ...

## Behaviors Non-Normative to Conventional ASDF Systems

* FASL files are generated under `"xml:bin;"`


## Source Trees

*   [de.setf.utility][utility]

    *   The [de.setf.utility][utility] _source tree_ provides a number
        of _system definitions_ used in CL-XML
        
    *   The file [hierarchical-names.lisp][hiernames] -- located in the
        [asdf][util-asdf] module of [de.setf.utility][utility] _source
        tree_ -- provides an interface for _hierarchical pathnames_ in
        locating [ASDF][asdf] system
        definitions, via `asdf:*system-definition-search-functions*`.
        
    *   Considering that the convention of _hierarchical
        pathnames_ applied in that extension is used throughout the
        `de.setf.*` source trees, it may _behoove the application_ of
        CL-XML, to apply that extension for [ASDF][asdf]. Notes:
        
        * The _hierarchical pathnames_ extension does not integrate
          with ASDF's newer _source registry_ mechanism

        * The _hierarchical pathnames_ extension essentially combines
          _pathname_ elements with _system name_ elements, for
          _resolving_ a _system name_ provided to
          `asdf::sysdef-hierarchical-search-function` as defined in
          [de.setf.utility][utility].

        * There is some special handling for system names ending with
          the suffix `-test`. See also:
          `asdf::sysdef-hierarchical-search-function`

        * At least superficially, it might resemble something with
          regards to pathname resolution for Java _class loaders_.

        * This extension may not be integrated directly with
          _source tree versioning_ systems.

            * With this extension, it may not be sufficient to add the 
              directory of a _source tree_ `A` to
              `asdf:*central-registry*` if it would be sought that a
              system definition for a system named `A.B` would be 
              located via this extension -- for example, with `A =
              de.setf.utility` and `B = mime`. Rather -- in that
              instance -- a directory `C.D.E.F` must be defined, such
              for `D = de`, `E = setf`, `F = utility` and an arbitrary
              `C` such that `C` would be referenced as an element of
              `asdf:*central-registry*`. (It is assumed, in this
              example, that a file `A/B/B.asd` exists) _This is the
              approach that will be applied in the MCi-DNet projects_

            * The pathname resolution conventions applied in the
              _hierarchical pathnames_ extension would essentially
              require that each version-controlled _source tree_ would
              be _published_ within the local _filesystem_, in a
              manner specially as to be accessible to the _hierarchical
              pathnames_ extension, external to the original _source
              tree_. Although this can be resolved relatively easily
              on most UNIX-like systems -- such as in using the `ln`
              shell command or an analogous _system call_, to define
              such _symbolic links_ -- however, it may not be
              applicable on operating systems in which a comparable
              _symbolic linking_ method would not exist, for
              filesystem references.

        * This extension may be integrated with ASDF 3, albeit in an
          _ad hoc_ and _platform-centric_ manner -- but such as to not
          interfere with normal _source tree_ versioning -- namely, as 
          around the _legacy_ application of `asdf:*central-registry*`
          in ASDF 3. To provide an example of one possible _ad hoc_
          methodology, as such -- namely, for platforms allowing for
          _symbolic linking_ for file directories
          
            1. Beginning at an arbitrary `{ROOT}` directory, create a
               directory `{ROOT}/de/setf/`
               
            2. Create a _symbolic link_ from the baseline source tree
               directory of each `de.setf.{name}/`  _source tree_ to
               the directory `{ROOT}/de/setf/{name}/` 
               
            3. Ensure that the `{ROOT}/` directory is listed within
               `asdf:*central-registry*` in the _lexical environment_
               of any function call to
               `asdf::sysdef-hierarchical-search-function`
               
        * Of course, the previous _ad hoc_ methodology might be
          managed with a simple function `register-foo`,
          provided a `{ROOT}` defined within the lexical environment
          of the same function. Some concerns, however:
          
            * The matter of _symbolic linking_ might provide some
              obstacles insofar as _portability_ of the methodology.

            * There may be a concern as with regards to
              _synchronization_ between the source tree `A.B.C/` and
              the subdirectories of `{ROOT}/A/B/C/`

        * Alternate to the previous _ad hoc_ methodology: A function
          `register-bar` may be defined, such that would search for
          system definitions within the directory `A.B.C/` and its
          subdirectories, and would register each system for its
          appropriate name, within `asdf/source-registry:*source-registry*`. If -- in
          that methodology -- if there might be some concerns as with
          regards to when a system `A.B.C.D` would be defined in a
          file named `D.asd` rather than `A.B.C.D.asd` ... then,
          candidly, it may still be no more of a _commercially
          reusable_ methodology, no more than to sell potatoes for
          paperweights.

        * A third methodology may be developed -- albeit again, in
          applying the _platform-centric_ feature, _symbolic linking_
          in filesystem resources -- of _linking_ a file
          `{FOO}/A.B.C/D.asd` to a file `{BAR}/A.B.C.D.asd`, such that
          the directory `{BAR}` would be accessible to ASDF, as within
           the ASDF 3 _source registry_ procedures. This methodology
           would serve to integrate with the `\*source-registry\*`
           methodology defined in ASDF 3, and would not require
           creation of additional filesystem directories, excepting
           the directory `{BAR}`. However, it would again require the
           _platform-centric_ feature, _symbolic linking_, insofar
           as _filesystem resources_, and it might present a concern
           as with regards to ensuring that the source tree `{FOO}`
           would remain synchronized with the registry directory
           `{BAR}`, insofar as with regards to ASDF system definitions
           created under `{FOO}`.

    *   For bootrstrapping the _hierarchical names_ extension,
        towards some of its original application:

        * The following example will apply a _logical pathname host_
          `"de.setf.utility"` defined as to translate to the pathname of 
          a local _clone_ of the [de.setf.utility][utility] *source
          tree*
        
        *   Referencing [build-init][build-init],
            the following files should be loaded in sequence, to
            load [hierarchical-names.lisp][hiernames]

            * `de.setf.utility:package`
            * `de.setf.utility:pathnames`
            * Lastly, `de.setf.utility:asdf;hierarchical-names`
            
        * Then, there are still the concerns as with regards to
          _pathname elements encoded in ASDF system names_, and
          _system registration external to original source tree_
    
*   [de.setf.atn-parser][atn-parser]

*   [de.setf.xml][cl-xml]

...

[utility]: https://github.com/lisp/de.setf.utility
[hiernames]: https://github.com/lisp/de.setf.utility/blob/master/asdf/hierarchical-names.lisp
[util-asdf]: https://github.com/lisp/de.setf.utility/tree/master/asdf
[asdf]: http://common-lisp.net/project/asdf/
[build-init]: https://github.com/lisp/de.setf.utility/blob/master/build-init.lisp
[atn-parser]: https://github.com/lisp/de.setf.atn-parser
[cl-xml]: http://de.setf.xml/
[cl-http]: http://www.cl-http.org:8001/

<!--  LocalWords:  hiernames asdf util pathnames ASDF's pathname de -->
<!--  LocalWords:  versioning setf filesystem ln hoc asd -->
<!--  LocalWords:  centric subdirectories reusability CLtL -->
<!--  LocalWords:  classpath codebases bootrstrapping init -->
<!--  LocalWords:  atn xml -->
