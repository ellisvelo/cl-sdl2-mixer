(cl:in-package #:sdl2-ffi)

(autowrap:c-include
 `(sdl2-mixer autowrap-spec "SDL_mixer.h")
  :function-package :sdl2-ffi.functions
  :spec-path '(sdl2-mixer autowrap-spec)
  :exclude-sources ("/usr/local/lib/clang/([^/]*)/include/(?!stddef.h)"
                    "/usr/include/"
                    "/usr/include/arm-linux-gnueabihf"
                    "/usr/local/include/SDL2"
                    #+darwin "/opt/homebrew/include/SDL2")
  :sysincludes (cl:append
                 #+darwin '("/opt/homebrew/include")
                 (cl:if (uiop:getenv "C_INCLUDE_PATH")
                   (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":")))
  :include-sources ("SDL_mixer.h")
  :sysincludes `,(uiop:split-string (uiop:getenv "EXTRA_INCLUDES") :separator " ")
  :exclude-constants ("^(?!MIX)")
  :symbol-exceptions (("SDL_RWops" . "SDL-RWOPS"))
  :no-accessors cl:t
  :release-p cl:t)
