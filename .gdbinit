# uses xxd to dump a memory region
# requires the address and a number of bytes
# the offset isn't preserved in the output
define xxd
  dump binary memory /tmp/gdb.xxd.bin $arg0 $arg0+$arg1
  shell xxd /tmp/gdb.xxd.bin
end
