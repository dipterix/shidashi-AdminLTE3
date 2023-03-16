The AdminLTE 3.2.0 (npm version) has the following bugs:

> `IFrame` registers listeners twice if called explicitly. (see https://github.com/ColorlibHQ/AdminLTE/issues/4520)

The solution is to set a flag in the element to register. If initialized on this element before, then abort. This change affects `js/IFrame.js`

```js
_setupListeners() {
  const initialized = $(this._element).data("setupListeners_initialized")
  if(initialized !== undefined){
  	return
  }
  ...
}
```

> The `scss/mixins/_background.scss` had an bugged `@mixin background-variant($name, $color)`

This style rule can set `&.bg-#{$name}` on the top level. This is explicitly forbidden by the syntax, because there is no selector reference to `&` on the top level.

The author has fixed (as of [commit 7748b3 ](https://github.com/ColorlibHQ/AdminLTE/commit/7748b38a6e7afc2415975b9c41adab4e1733d330)). Therefore the fix is to use the newer version.

