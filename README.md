mci-dnet-xsd - Implementation of XML Schema Data types
======================================================

## Overview

### Historic Context: Project Lupine

This project represents an effective descendant of a feature of a
project originally named _Lupine_, insofar as extending of the design
for an implementation of XML schema data type structures, as
originally defined within the Lupine project. Lupine was represented
primarily of a set of UML models, defined for a goal of implementing
XML schemas, XMI, MOF, and UML within Common Lisp. Lupine has been
subsequently revised as the set of projects organized under the
'mci-dnet' workspace. 

### Implementation of XML Schema Simple Data types

This system implements a facet-oriented view of XML Schema simple data
types.

Fundemtnal Facets in terms of XML schema syntax:

* **cardinality:** finite | countably infinite
* **numeric:** true | false
* **bounded:** true | false
* **ordered:** partial | total | (false?)

This sytem defines a number of protocol classes for purpose of
representing those facets of XML schema simple data types. Those
protocol classes are defined as to allow for accurate representation of
XML schema data type definitions, within the Common Lisp
environment. The class precedence list of each protocol class, as a
quality essentially orthogonal to the XML Schema specification, is 
defined as to ensure a manner of semantic accuracy in the definition
of the respective data type definition.


Type Structure in mci-dnet-xsd (draft 1 - incomplete)

_Ed. note: Effectively, the structure of relations of class
definitions represented in the original Lupine  XSD simple data types
design will serve to require a documentation structure other than a
single outline. There should be at least three sets of classes listed:_

* _Protocol classes_ (`protocol-class`)
* _Faceted protocol classes_ (`faceted-class`)
    * Relevance: _Fundamental facets_ of XML Schema data types
    * Facet: 'cardinality' 
        * finite: `finite-type` 
        * countably infinite: `countably-infinite-type`
    * Facet: 'numeric'
        * true: `numeric-type`
        * false: `non-numeric-type`
    * Facet: 'bounded'
        * 
    * Facet: 'ordered'
        * `ordered-type` (protocol class)
            * partial: `partially-ordered-type`
            * total: `ordinal-type`
        * false: `non-ordered-type`


* _Simple type classes_ (`simple-type-class`)

_Additional focus should be made about the set of numeric types
defined in XML Schema Data types. Seperately, a note should be made
with regards to enumerable simple types in XSD__

_Application Note: XML schema data types find a relevance in encoding
of XML schemas, and furthermore in RDF XML_

_Architectural Note: This system should endeavor to extend of the
metaobject protocol, in implementation of XML schema data types._

_Architectural Note: This system may also endeavor to establish a
certain semantic homology onto CORBA -- viz a viz CLORB -- as
specifically to define a  generic protocol for marshalling and
unmarshalling of values encoded in syntaxes other than ANSI Common
Lisp. That would be essentially orthogonal to the structures of the
class hierarchy defined in mci-dnet-xsd_


_Incomplete Oultline_ (TO DO: Reference original Cubetto UML model)

* `type`
    * `complex-type` (TBD) (cf. CXML)
    * `simple-type`
        * `finite-type`, facet 'cardinality': finite
            * `bounded-type`, facet 'bounded' : true
        * `non-bounded-type`, facet: 'bounded' : false
        * `countably-infinite-type`, facet 'cardinality': countably infinite
            * `numeric-type`, facet 'numeric': true
                * slots: max-exclusive, min-exclusive, max-inclusive, min-inclusive
                * TBD: Determine "best practice" for canonicalization of exclusive/inclusive limits in interpretation of type definitions
                * `decimal` (simple type class)
                    * also an `ordinal-type`, `non-bounded-type` type
                    * `integer` (simple type class)
                        * `non-negative-integer` (simple type class)
                        * `bounded-integer` (protocol class)
                            * also a `bounded-type` type
                            * `unsigned-bounded-integer` (protocol class)
                                * also a `non-negative-integer` type
                            * `signed-bounded-integer` (protocol class)
               *  ... (TO DO: Transpose remaining classes from Lupine model, see previous notes)
        * `enumerable-type` (protocol class)
            * `ordered-type` (protocol class)
            * `literal-type` (simple type class)
                * slots: length, max-length, min-length
        * `non-numeric-type`, facet 'numeric' : false
            * `non-ordered-type` (protocol class)
        * `ordered-type` (protocol class)
            * `ordinal-type`, facet 'order' : total
            * `partially-ordered-type`, facet 'order': partial

### Implementation of XML Schema Complex Data Types

(TO DO, cf. CXML, Klacks, Sax, XML DOM, and MOP)
