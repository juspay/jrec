# jrec — anonymous records for busy people

Based on [superrecord](https://hackage.haskell.org/package/superrecord), but simplified. No automatic field reordering. No GHCJS bits inside. Easier to hack on.

## Benefits

* Nice construction syntax:

  ```haskell
  Rec (#id := 18853, #orderId := "MG13-233")
  ```

* Provides `Generic` instances out of the box. Aeson, etc can't believe these aren't normal records! Anything `Generic`-derived just works.

* Provides `generic-lens` and `generic-optics` instances out of the box. Due to those custom instances, we support polymorphic updates.

* O(1) field access, O(n) construction.

* PureScript-style partial records — if you have a `Rec ("foo" := Int ': rest)`, `HasField "foo"` will work just fine.

## Developing

IDE support is available inside `nix-shell`. For example, if you use VS Code, you may launch it as:

```bash
nix-shell --run "code ."
```

Run tests inside ghcid using `bin/test`.

## TODOs

* Documentation.
* Expose all internals.
* `-- NOTE: doesn't use 'KeyDoesNotExist'` — fix this.
* `RemoveAccessTo` — can we get rid of it?

## Acknowledgement

* The `JRec.Internal` module is entirely based on the code from [superrecord](https://hackage.haskell.org/package/superrecord).
