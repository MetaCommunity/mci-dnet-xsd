mci-dnet-xsd - Implementation of XML Schema Data types
======================================================

## Overview

### Introduction

* XML
* XML Schema Documents (XSD)
* Other Types of Schema for XML
    * DTDs - Subset of original SGML DTD expression languag
    * RELAX NG - XML or Compact Syntax
    * ...

#### Usage Cases

* **Complex Types in XSD** - Transform XML Schema Definitions for
  [XMI](http://www.omg.org/spec/XMI/) into Common Lisp Class
  Definitions, towards implementaion of an XMI serialization framework
  for an implementation of the Metaobject Framework (MOF), Unified
  Modeling Language(UML), Systems Modeling Langauge (SysML), ODM, and
  other standard metamodels, in Common Lisp 
* **Simple Types in XSD** - Support definition of XSD simple types for
  application in RDF graph files

### Dependencies

* `info.metacommunity.cltl.utils`
* `closer-common`

### XML Names Dictionary

* Function `NCNAME-P`
* Function `NMTOKEN-P`
* Function `QNAME-P`
* Type `NCNAME`
* Type `SIMPLE-NCNAME`
* Type `NMTOKEN`
* Type `SIMPLE-NMTOKEN`
* Type `QNAME`
* Type `SIMPLE-QNAME`

### XML Namespaces

The XML Schema 1.1 abstract data model
[[XSD 1.1 part 1][XSD11-1]][[XSD 1.1 part 2][XSD11-2]]
allows for schema authors to apply XML namespaces
[[XML-NAMES][xml-names]] within definitions of XML Schema
structures and XML Schema data types. Similarly, the DOM Level 2 
[[DOM2C][DOM2]] and DOM Level 3 [[DOM3C][DOM3C]] _core_ specifications
permit for a URI value to be provided, when querying a _named node
map_. In any regards, the namespace of an XML element may be
considered relevant to applications in processing of _well formed_ XML 
infosets [[XML Infoset][XML-Infoset]].

In a context of qualified names within XML documents: When a namespace
is declared within an XML element, that namespace remains _in scope_ 
for its declaration in that XML element -- whether the namespace is
declared as a default namespace or as a qualified namespace --
remaining _in scope_ until either the namespace declaration is
shadowed by a _similar_ namespace declaration within a contained
element, or until the end of the element's tag body. In a context of
XML documents, an XML element effectively is a container for namespace
declarations. 

In a context of _target namespaces_ within XML schema structures
\[[XSD 1.1 part 1][XSD11-1]\] and XML schema data types
\[[XSD 1.1 part 2][XSD11-2]\], the _targetNamespace_ attribute may be
specified in a definition, such as to denote the namespace for an
element, attribute, or other information item defined in the
definition.

If the _targetNamespace_ of an element _D1_ must be referenced by way
of a _QName_ value within an element _D2_, then the
_targetNamespace_of _D1_ must be bound to a _namespace prefix_ by way
of an appropriate _xmlns_ attribute within a context of
_D2_. Although this is not expressly stated within the specification
for XML schema structures [[XSD 1.1 part 1][XSD11-1]], but this
requirement would be consistent with the nature of XML namespace
declarations [[XML-NAMES][xml-names]], such that would provide a
consistent mechanism for defining a _namespace prefix_ to a _namespace
URI_.


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


[XSD11-1]: http://www.w3.org/TR/xmlschema11-1/
[XSD11-2]: http://www.w3.org/TR/xmlschema11-2/
[XML-NAMES]: http://www.w3.org/TR/REC-xml-names/
[DOM2C]: http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113/
[DOM3C]: http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/
[XML-Infoset]: http://www.w3.org/TR/xml-infoset/
