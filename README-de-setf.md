
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
              example, that a file `A/B/B.asd` exists)

            * The pathname resolution conventions applied in the
              _hierarchical pathnames_ extension would essentially
              require that each version-controled _source tree_ would
              be _published_ within the local _filesystem_, in a
              manner specially as to be accessible to the _hierarhical
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
          managed with a simple function `reigster-foo`,
          provided a `{ROOT}` defined within the lexical environment
          of the same function. Some concerns, however:
          
            * The matter of _symbolic linking_ might provide some
              obstacles insofar as _poratbility_ of the methodology.

            * There may be a concern as with regards to
              _sychronization_ between the source tree `A.B.C/` and
              the subdirectories of `{ROOT}/A/B/C/`

        * Alternate to the previous _ad hoc_ methodology: A function
          `register-bar` may be defined, such that would search for
          system definitions within the directory `A.B.C/` and its
          subdirectories, and would register each system for its
          appropriate name, within `asdf:*source-registry*`. If -- in
          that methdology -- if there might be some concerns as with
          regards to when a system `A.B.C.D` would be defined in a
          file named `D.asd` rather than `A.B.C.D.asd` ... then,
          candidly, it may still be no more of a _commercially
          reusnable_ methdology, no more than to sell potatoes for
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

        * In a candid summary of the author's views as with regards
          to this extension: Altogether, this extension may seem to
          make for an indeterminate amount of inconvenience for the
          developer -- as with regards to  _reusability_ of this
          extension -- such that may be weighed against the merits of
          the methogolody provided, with this extension, for naming
          and location of system definitions. Although any single
          _methodology_, as of the previous, may be applied as an
          effective _work around_ for systems distributed to _end
          users_  -- such that any single _end user_ would not be
          expected to modify the systems as distributed, in any manner
          that would interfere with any functional applications of the
          specific methodology -- it may seem to amount to an
          indeterminite uncertainty for applications and application
          developers, moreover as a sort of _virulent_ methodology
          establishing no too certain analogy to Java classpath
          handling, in a contrivance in which _system naming_ becomes
          inherently confounded with _system pathname resolution_ --
          and this, if only for a re-use of such existing codebases as
          would apply such a convention as effectively for encoding of
          _system relative pathname in system name_ -- furthermore, it
          introducing an ultimately inconvenient sense of ambiguity to
          the concept of _system name_ in ASDF. For such a simple 
          contrivance in _system definition naming and location_, it
          may seem to _ask too much_, certainly too much to be
          deemed ultimately _convenient_ for _re-use_ -- though
          clearly, it is _novel_. In as it reminds of a programming
          system much pragmatically unlike Common Lisp -- namely, the
          Java Development Kit -- but it becomes a _novel
          distraction_, moreover, for the features that this simple
          extension _does not provide_, in any analogy to features
          innately available of a Java Development Kit -- such as with
          regards to  programmatic security  policies for the Java
          class loader, also as with regards  to _Java archives_, _and
          so on_. It becomes like a distracting excercise in _rote
          tedium_, then, to apply this extension and not resolve the
          concerns introduced of this extension, if those may be
          resolved to any further manner of a pragmatic conclusion. In
          the author's point of view: It begs a metaphor like of an
          _entrenching tool_ -- albeit, perhaps a magical entrenching
          tool that can be extended to the farthest reaches of the
          sky, if it were -- but that is nothing of a metaphor the
          author would wish to apply for a 
          systems programming task. Ideally, it may be far simpler
          to use a _flat naming scheme_ for system definitions, and
          not require  that developers would review any specific
          extensions to ASDF, whether or not to re-use those same
          extensions, if to simply re-use an existing software
          component designed originally as to require the same
          extensions for ASDF. All of this becomes an unremarkable
          sidebar, then, as it is clearly not sufficient to apply an
          existing convention of _flat system names_ for ASDF system 
          definitions -- insofar as with _some ASDF system
          definitions_ -- though the latter is really a concise
          convention. It is, then, another plainly exasperating
          quality of the effective Common Lisp programming
          environemnt, moroever another barrier to any -- at least, in
          a _hypothetical_ sense -- any further commercial adoption of
          this innately non-integrated, remarkably heterogenous heap
          of _software things_ that is become in applications of
          CLtL2, much to the detriment of any practical resuability.

    *   For bootrstrapping the _hieararchical names_ extension,
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
