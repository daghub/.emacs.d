; Example config file for clang auto complete
(setq sp-include-dirs
  '("targets/Darwin-x86_64-debug"
    "."
    "client"
    "src"
    "client/base/lib"
    "targets/Darwin-x86_64-debug/obj/log-parser"
    "targets/Darwin-x86_64-debug/obj/boink"
    "targets/Darwin-x86_64-debug/obj/protobuf"
    "targets/Darwin-x86_64-debug/obj/passive_boink"
    "client/boink/cpp"
   )
)


(setq sp-compile-flags
  '("-D_DEBUG"
    "-DOVERRIDE"
    "-D__cplusplus"
    "-DSP_LIBSPOTIFY=1"
    "-DSP_WITH_SOCIAL=1"
    "-DSP_LIBSPOTIFY_WITH_SOCIAL=1"
    )
)
