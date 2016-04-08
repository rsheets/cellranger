---
title: "Classes and methods related to cells"
---

## Reference

Spreadsheet Implementation Technology: Basics and Extensions  
Peter Sestoft  
https://mitpress.mit.edu/books/spreadsheet-implementation-technology

## Informal intro to cell references

1.3 Cell reference formats, p. 3

|     | A1   | R1C1       |
|-----|:-----|:-----------|
| abs | $B$4 | R4C2       |
| rel | B4   | `R[4]C[2]` |

Two main points:

  * A1 vs. R1C1 format (I've said *notation* in the past but will switch to *format*)
  * References can be absolute or relative
  
Two subtleties the table above glosses over:

  - relative to which cell?
  - a cell reference can be mixed, e.g. relative for the column but absolute for the row
  
By default, an A1-formatted cell reference is relative. Use a dollar sign `$` to indicate absolute.

By default, an R1C1-formatted cell reference is absolute. Relative references are indicated by putting the offset inside square brackets. Except when the offset if zero, when it is left out. So `RC` is how to say "this cell" -- not `R[0]C[0]`.

We like R1C1 format, especially for relative references, because it is invariant when you copy a formula. And it is just more sane, in every possible sense, actually.

Meaning of cell references **assuming found in cell B2**

| A1   | R1C1         | Meaning                     |
|:-----|:-------------|:----------------------------|
| A1   | `R[-1]C[-1]` | rel: prev row, prev col     |
| A2   | `RC[-1]`     | rel: this row, prev col     |
| B1   | `R[-1]C`     | rel: prev row, this col     |
| B2   | `RC`         | rel: this cell              |
| C3   | `R[1]C[1]`   | rel: next row, next col     |
| $A$1 | `R1C1`       | abs: row 1, col 1 or A      |
| $A2  | `RC1`        | mixed: this row, col 1 or A |
| C$3  | `R3C[1]`     | mixed: row 3, next col      |

![](cell-reference-diagram.jpg)

## Important concepts in Sestoft's Corecalc spreadsheet implementation

From Peter Sestoft's book Spreadsheet Implementation. Section 2.1.1, p28-->.

A subset of the core concepts that seem immediately relevant to us:

  * A *workbook* of class Workbook is a collection of sheets
  * A *sheet* of class Sheet is a rectangular array (*ok, not how we are thinking of it?*), each element could contain null or a cell
  * A (non-null) *cell* of class Cell which could be a constant (he's got more classes here), a formula (that's got its own class, see below), an array formula (I'm trying to pretend these don't exist right now). (*This is where he says a cell could also contain info about formatting but in his world it does not.*)
  * A *formula* of class Formula consists of an expression (that's got a class), a cached value (again, a class), a workbook reference of class Workbook, a state (has to do with recalculation).
  * An *expression* of class Expr could be a constant (number, text), a static error, an object of class CellRef, an object of class CellArea, or a call to a function or operator (of class FunCall). Can be nested.
  * A *raref* of class RARef, which is a cell reference. A pair of integers `colRef` and `rowRef` specifying the column and row PLUS a pair of Booleans `colAbs` and `rowAbs` which indicate whether the associated reference is absolute vs. relative.
  * A *cell address* of class CellAddr is an absolute location of a cell within a sheetl, i.e. `(row, col)`. For Sestoft, it's zero-based.
  * A single cell reference of class CellRef is an object of class RARef and, optionally, a worksheet reference.
  * A cell area reference of class CellArea consists of two objects of class RARef (upper left, lower right of a rectangle) and, optionally, a common worksheet reference.

## Cell references

The contents of a cell must be either:

  * a constant (number, string, etc.)
  * a formula, which is an equals sign `=` followed by an expression
    - a formula can contain references to other cells, operators, and function calls

How do we go from the string representation of a formula (concrete syntax) to the internal representation (abstract syntax)? Formula text is first scanned or tokenized, then it is parsed into an abstract syntax tree (AST). Sestoft presents a full grammar for cell contents (p34) but here I just focus on the cell references.

Here's what cell references look like in the string representation:

```
[WORKBOOK.xlsx]'WORKSHEET NAME'!A1
```

*Note: Sestoft never mentions the possibility of the filename appearing at the front, but that's definitely possible in Excel.*

Here's the relevant subset of the Sestoft cell contents grammar:

```
Expr ::=
  Raref
| Raref : Raref

Sheetref ::=
  Name ! Raref
| Name ! Raref : Raref

Raref ::=
  Column Row
| $ Column Row
| Row $ Column
| $ Column $ Row
| R Offset C Offset

Offset ::=
  <empty>
| Uint
| [ Int ]
```

where `Column` is a column name A, B, ...; `Row` is a row number 1, 2, ...; `Uint` is a non-negative integer; and `Int` is an integer.

*Sestoft doesn't allow 3-D cell references, e.g. `Sheet1:Sheet12!B2`, and I don't plan to deal with them either.*

*Sestoft doesn't allow for full-column references, e.g. `B:B`, but I think I need to. Not sure why he doesn't mention full-row references.*

Sestoft describes full-on scanning/tokenization + parsing for string representations of cell contents (p33); this returns an AST for each cell as an object of class `Cell`. In `cellranger` we only deal with cell references and I'll just use regular expressions.

Revisiting classes relevant to `cellranger`:

  * RARef for a single cell reference:
    - `colAbs` and `rowAbs` are logical indicators of absoluteness
    - `colRef` and `rowRef` specify row and column, in an absolute or relative sense
  * CellRef is a RARef and, optionally, a sheet reference.
  * CellArea is two RARefs (the corners) and, optionally, a sheet reference.
  * CellAddr is pair of non-negative integers giving sheet-relative, absolute cell address.

## Methods

The Addr method for a RARef returns a CellAddr. English: single cell reference goes in, (row, column) pair comes out. FYI, the result of this is always used in a context where sheet info is available separately, e.g. from the CellRef object that contained the associated RARef.

The constructor for a CellAddr takes the row and column of the host cell and a RARef and returns absolute cell address that the RARef points to.

The ToString method for CellAddr take an absolute cell address and returns a typical string representation. Looks like Sestoft always returns in A1 format in this case. Why?

The Show method for a RARef generates one of the usual string representations of a cell reference. Sestoft's Show takes row and column (so he Shows only after applying Addr method, I guess?) and a format (A1 vs R1C1) as input.

A CellRef is a valid Expr in Sestoft's Corecalc. When you Eval it, the cell address gets resolved (row, column, sheet) and the formula in that cell is evaluated. We obviously won't do all of that in `cellranger` (i.e. the formula retrieval and evaluation), but should do the cell resolution part. I think we need to a function that takes a host cell and a (potentially relative) cell reference and gives back another (absolute) cell reference. File and sheet reference (or lack thereof) should propagate.

The Show method for a CellRef builds up its output from the the Show method for a RARef and the sheet reference, if such exists.

## Mixed cell area reference

Example given in 1.4 Formulas, functions, and arrays p. 5

Consider formula `=SUM(A$1:A1)` in the absolute cell `B1`.

I should run through what happens when you move/copy that formula into `B2` or `B3` or column `C`. *Come back to this.*
