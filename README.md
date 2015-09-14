# Clash6502

A naive implementation of a MOS 6502 in Clash. It is not cycle accurate nor very efficient in terms of space usage.
It's intened to be a simple none trivial project to better understand Clash.

Currently it implements all documented 6502 instructions and passes a basic functional test.
There is no current support for external interrupts.

The Makefile is specific to my odd setup developing on a Mac with the Xilinx tools hosted in a Windows VM run via SSH from the Mac referencing source files via a network share.



