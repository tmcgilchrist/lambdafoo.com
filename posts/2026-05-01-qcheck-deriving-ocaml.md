---
title: "Deriving QCheck Generators for External Types in OCaml"
date: 2026-05-01
tags: [ocaml, testing, qcheck, quickcheck, ppx]
---

Recently I've been working on [durin](https://github.com/tmcgilchrist/durin), a DWARF library for OCaml. I won't go into the details here but I wanted to share a property testing technique I've been using in durin.

The [DWARF spec](https://dwarfstd.org) is huge (version 5 is just short of 500 pages) and includes many large variant types that can be combined in different ways. To test the serialisation and deserialisation code I'm using property testing, and in particular a technique I picked up working with [QuickCheck](https://github.com/nick8325/quickcheck) in Haskell that I haven't seen written up for OCaml. Let's look at how to derive generators for our types using [QCheck](https://github.com/c-cube/qcheck), OCaml's QuickCheck-inspired property-based testing library.

## Deriving QCheck Generators

When writing property-based tests in OCaml, you need QCheck generators for the library's types. These generators then get used when you write your tests. The [QCheck documentation](https://github.com/c-cube/qcheck#an-introduction-to-the-library) is a good introduction to the library.

You have a couple of options for where you add these generators. The obvious approach is adding `[@@deriving qcheck]` directly to the library's type definition. This uses a PPX (which you can think of as a macro that generates new code before it gets compiled), which pulls `qcheck-core` and `ppx_deriving_qcheck` into the library's dependency tree. This is unfortunate as every consumer of the library picks up a QCheck dependency whether they need it or not.

The second approach would be deriving something like `[@@deriving enum]` to generate `to_enum`/`of_enum` conversions for the variants you want to test, and then writing the QCheck generators by hand in the test code using those conversions. This improves on the previous approach as you avoid library consumers needing QCheck, but you still have the downside of needing a PPX dependency on your library. This might be totally fine if you're already using PPX elsewhere in the library.

Finally you can write the generator manually, which you often want to do with types that have properties that are hard to encode directly in the type. For example, when generating a date you might want to bias the generation to use modern dates between 1990 and 2030. For now let's assume you mostly want automatically derived generators.

This post shows a technique that keeps the library free of PPX and QCheck dependencies while still getting derived generators in test code.

## The problem

Say you have a library with variant types like these from durin:

```ocaml
(* lib/dwarf_constants.ml *)
type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

type endianity =
  | DW_END_default
  | DW_END_big
  | DW_END_little
  | DW_END_lo_user
  | DW_END_hi_user

(* lib/dwarf_form.ml *)
type attribute_form_encoding =
  | DW_FORM_addr
  | DW_FORM_block2
  | DW_FORM_data1
  (* ... 45 more constructors ... *)
  | DW_FORM_unknown of int  (** Unknown or vendor-specific forms *)
```

Now you want to apply the roundtrip test pattern like `decode(encode(v)) = v` for every variant. For durin I want to ensure I get the same value back that I passed to my serialisation function.

In this case you need a QCheck generator for each type. You might even need a generator for types that you didn't define and are coming from other libraries. How can we handle this?

### Library provides generators

The library depends on `qcheck-core` and ships generators alongside each type:

```ocaml
(* lib/dwarf_constants.ml *)
type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

let gen_accessibility =
  QCheck.Gen.oneofl
    [DW_ACCESS_public; DW_ACCESS_protected; DW_ACCESS_private]
```

This gets the job done, but every user of the library now transitively depends on `qcheck-core`. The library's `.opam` file grows and build times increase for everyone, not just people running tests. Often in Haskell I would define a second library called `library-test` to ship alongside the main library, bundling the generators and any associated test utilities. This is fine for code you aren't shipping to opam (maybe it's even fine there but doesn't seem to be the done thing). What else can we try?

### Library uses ppx_deriving_qcheck

Rather than writing and shipping generators directly, the library adds the PPX deriver annotation `[@@deriving qcheck]`.

```ocaml
(* lib/dwarf_constants.ml *)
type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private
[@@deriving qcheck]
```

This is a variation of the previous approach where the deriver writes the generators for you. It saves developer time, and you get generators even for types you might not have written by hand. The library now depends on both `qcheck-core` and `ppx_deriving_qcheck` at build time. PPX dependencies tend to be quite heavy, the PPX machinery for deriving is shipped separately from the compiler even though it's using the OCaml AST. By using the OCaml AST your project needs to be more careful about the OCaml versions it uses, and it can be difficult to use your code on pre-release versions of OCaml. Which I tend to do a lot as I work on the compiler. So it makes sense to avoid a PPX dependency in certain cases.

### Tests derive generators externally

What I want is for the test code to only be required when testing. The main library knows nothing about QCheck, while the test executable imports the type definitions and derives generators locally. The PPX dependency is confined to the test executable. This approach also leaves the door open to shipping the generators as a separate `library-test` package. Before getting into how to wire this up in OCaml, let's look at the same problem in Haskell.

## The Haskell analogy

If you've used QuickCheck in Haskell, this problem is familiar. When a library defines a type but doesn't provide an `Arbitrary` instance (the equivalent to a generator), you can't write an Arbitrary instance for it directly in test code. GHC warns about orphan instance and the ecosystem generally discourages orphan instances. The standard workaround for this problem is a newtype wrapper.

```haskell
-- Library defines:
data Color = Red | Green | Blue

-- Test code:
newtype TestColor = TestColor Color
  deriving newtype (Eq, Show)

instance Arbitrary TestColor where
  arbitrary = TestColor <$> elements [Red, Green, Blue]

prop_roundtrip :: TestColor -> Bool
prop_roundtrip (TestColor c) = decode (encode c) == c
```

The newtype is zero-cost at runtime but gives you a place to hang the instance without orphan warnings. The key insight is the type definition is structurally duplicated in test scope so the typeclass machinery can operate on it. Haskell also has a built-in deriving mechanism similar to OCaml's PPX.

## Solving with QCheck

OCaml doesn't have typeclasses or orphan instances, but the problem is structurally identical. A PPX deriver needs to see the type definition in the file where it runs. If the definition is in the library and the library doesn't use the PPX, the deriver never sees it.

Enter `ppx_import` with a clever solution. It copies a type definition from a compiled module interface file (`.cmi`) into the current file at pre-processing time. A `.cmi` is the OCaml compiler's binary form of an `.mli` interface, holding type information and module signatures but no implementation. If you want the bigger picture of what the compiler produces, the [OCaml compiler toolchain docs](https://ocaml.org/docs/using-the-ocaml-compiler-toolchain) cover the other artifacts (`.cmo`, `.cmx`, `.cma`, `.cmxa`, and friends).

```ocaml
(* In test code, NOT in the library *)
type accessibility = [%import: Durin.Dwarf.accessibility] [@@deriving qcheck]
```

After preprocessing, this expands to:

```ocaml
type accessibility =
  | DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

let gen_accessibility =
  QCheck.Gen.oneof
    [ QCheck.Gen.pure DW_ACCESS_public;
      QCheck.Gen.pure DW_ACCESS_protected;
      QCheck.Gen.pure DW_ACCESS_private ]
```

The imported type is structurally identical to `Durin.Dwarf.accessibility` which OCaml can unify. No newtype wrapper, no coercion, no runtime cost. Unlike Haskell, there's no orphan instance concern because OCaml generators are plain values, not typeclass instances. We get all the benefits of QCheck and PPX without making our library users depend on them.

One caveat worth flagging. `ppx_import` can only copy type definitions that are actually exposed in the library's `.mli`. If a type is abstract (declared as `type foo` with no right-hand side), there is nothing for import to copy and you'll need to fall back to a hand-written generator in the test code. At that point you would be questioning why you don't have access to the type and if it should be present in the `.mli` file.

## Step-by-step setup

This is how I've been setting up this pattern in durin.

### Add test-only dependencies

In your `dune-project`, add the PPX packages scoped to tests:

```
(package
 (name durin)
 (depends
  (ocaml (>= 5.3))
  ;; ... your library dependencies ...
  (ppx_import :with-test)
  (ppx_deriving_qcheck :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))
```

The `:with-test` scope means these packages are never required by library consumers. They're only installed when running tests or installed with `opam install --with-test`.

### Configure the test stanza with staged_pps

In your `test/dune`:

```
(test
 (name test_roundtrip)
 (libraries durin alcotest qcheck-core qcheck-alcotest)
 (preprocess (staged_pps ppx_import ppx_deriving_qcheck)))
```

I'm using `staged_pps` rather than plain `pps` because `ppx_import` requires two passes. The first pass runs through ocamldep to work out which modules the file depends on, and only the second pass has the `.cmi` files available to copy the type definitions from. Regular `pps` runs in a single pass where `ppx_import` won't have the `.cmi` files available yet.

### Import types and derive generators

Import the types using ppx_import and derive the qcheck generators:

```ocaml
(* test_roundtrip.ml *)
open Durin.Dwarf

(* ppx_import copies the type definition from the .mli.
   ppx_deriving_qcheck generates gen_<name> : <name> QCheck.Gen.t
   for each type. *)
type accessibility = [%import: Durin.Dwarf.accessibility]
[@@deriving qcheck]

type endianity = [%import: Durin.Dwarf.endianity]
[@@deriving qcheck]
```

This gives you `gen_accessibility : accessibility QCheck.Gen.t` and `gen_endianity : endianity QCheck.Gen.t` to use in your tests.

### Customise types with payloads

For variants carrying data, `ppx_deriving_qcheck` generates sub-generators automatically for standard OCaml types (`int`, `string`, `float`, `bool`, etc.):

```ocaml
(* Library defines:
   type attribute_form_encoding =
     | DW_FORM_addr
     | ...
     | DW_FORM_unknown of int *)
type attribute_form_encoding = [%import: Durin.Dwarf.attribute_form_encoding]
[@@deriving qcheck]

(* Generates a generator that picks one of the constructors,
   passing a generated int to DW_FORM_unknown:
   let gen_attribute_form_encoding =
     QCheck.Gen.oneof
       [ QCheck.Gen.pure DW_FORM_addr;
         ...
         QCheck.Gen.map (fun i -> DW_FORM_unknown i) QCheck.Gen.int ] *)
```

If the payload is another type from your library, import and derive the inner type first. The outer generator will then reference it by name:

```ocaml
type base_type = [%import: Durin.Dwarf.base_type] [@@deriving qcheck]
type dwarf_language = [%import: Durin.Dwarf.dwarf_language] [@@deriving qcheck]
type attribute_value = [%import: Durin.Dwarf.DIE.attribute_value] [@@deriving qcheck]
(* gen_attribute_value uses gen_base_type for the Encoding constructor
   and gen_dwarf_language for the Language constructor *)
```

Sometimes the default generator for a field is wrong, for example a DWARF version that must be 2 to 5. You can override it with a `[@gen ...]` attribute, but attributes don't survive the `[%import: ...]` expansion, so restate the record definition in test scope using OCaml's type sharing:

```ocaml
(* Library: type encoding = {
     format : dwarf_format;
     address_size : u8;
     version : u16;
   } *)
type encoding = Durin.Dwarf.encoding = {
  format : dwarf_format;
  address_size : u8;
  version : u16 [@gen QCheck.Gen.map Unsigned.UInt16.of_int (QCheck.Gen.int_range 2 5)];
}
[@@deriving qcheck]
```

The `type encoding = Durin.Dwarf.encoding = { ... }` form tells OCaml that `encoding` is the same type as `Durin.Dwarf.encoding`, so values flow between them without coercion. For simple type aliases where restating the definition doesn't help, skip the deriver and write the generator by hand:

```ocaml
type register = [%import: Durin.Dwarf.register]
(* AArch64 has 31 general-purpose registers *)
let gen_register =
  QCheck.Gen.map (fun n -> Register n) (QCheck.Gen.int_range 0 30)
```

### Write the actual tests

`ppx_deriving_qcheck` produces `QCheck.Gen.t`, not `QCheck.arbitrary`. Wrap with `QCheck.make` for use with `QCheck.Test.make` like so:

```ocaml
let arb gen = QCheck.make gen

(* A generic roundtrip helper *)
let roundtrip name gen encode decode =
  QCheck.Test.make ~name:(name ^ " roundtrip")
    (arb gen)
    (fun v -> decode (encode v) = v)

let tests =
  [
    roundtrip "accessibility" gen_accessibility
      int_of_accessibility accessibility;
    roundtrip "endianity" gen_endianity
      int_of_endianity endianity;
  ]

let () =
  let open Alcotest in
  run "roundtrip tests"
    [
      ( "roundtrip",
        List.map QCheck_alcotest.to_alcotest tests );
    ]
```

## A real-world example

Here's the complete test file from [durin](https://github.com/tmcgilchrist/durin), a DWARF debugging format library. It tests roundtrip properties for 26 DWARF constant types, each with from 2 to over 170 variants:

```ocaml
open Durin.Dwarf

(* Import and derive generators for every type under test.
   When a variant is added to any type in the library, the
   generator updates automatically. *)
type abbreviation_tag =
  [%import: Durin.Dwarf.abbreviation_tag]
[@@deriving qcheck]

type attribute_encoding =
  [%import: Durin.Dwarf.attribute_encoding]
[@@deriving qcheck]

type calling_convention =
  [%import: Durin.Dwarf.calling_convention]
[@@deriving qcheck]

(* ... 23 more types ... *)

let arb gen = QCheck.make gen

let roundtrip name gen encode decode =
  QCheck.Test.make ~name:(name ^ " roundtrip")
    (arb gen)
    (fun v -> decode (encode v) = v)

let tests =
  [
    roundtrip "abbreviation_tag" gen_abbreviation_tag
      uint64_of_abbreviation_tag abbreviation_tag_of_int;
    roundtrip "attribute_encoding" gen_attribute_encoding
      u64_of_attribute_encoding attribute_encoding;
    roundtrip "calling_convention" gen_calling_convention
      int_of_calling_convention calling_convention;
    (* ... *)
  ]
```

## Conclusion

This is a useful technique to add to your OCaml property-testing toolkit. It is self-maintaining, so when you add a variant to a type in the library, `ppx_import` picks up the new definition on the next build and `ppx_deriving_qcheck` generates a generator that includes it.

The library side stays clean. No PPX dependencies, no QCheck dependency, no generated code in its build artifacts. The `.opam` file and build are unchanged, and consumers who never run tests never see these test packages. The imported type is also the same OCaml type as the library's, so there are no type-safety issues to worry about. It keeps open the option for defining a test library that just includes useful generators.

As a bonus, the same approach lets you define generators for types outside of your control, which is handy when you want to test round-trips against data from another library. I've found this pattern very useful and I hope you do too.
