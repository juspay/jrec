# rec — anonymous records for busy people

Based on [superrecord](https://hackage.haskell.org/package/superrecord), but simplified. No automatic field reordering. No GHCJS bits inside. Easier to hack on.

## Benefits

* Nice construction syntax:

  ```haskell
  Rec (#id := 18853, #orderId := "MG13-233")
  ```

* Provides `Generic` instances out of the box. Aeson, etc can't believe these aren't normal records! Anything `Generic`-derived just works.

* Provides `generic-lens` instances out of the box. Due to those custom instances, we support polymorphic updates.

* O(1) field access, O(n) construction.

## TODOs

* Documentation.
* Expose all internals.
* `-- NOTE: doesn't use 'KeyDoesNotExist'` — fix this.
* `RemoveAccessTo` — can we get rid of it?
