SECTION .text

; TODO: So far, this file is empty. The future purpose of this file
; will be to provide functions that call UEFI runtime services, so that'so
; functions like `SetVirtualAddressMap()` or `GetTime()`. I think I could
; do this just with Ada, but as of now I don't know how well it plays
; with different calling conventions. GCC is still the compiler, so I think
; it will be fine, but this is here just in case.
