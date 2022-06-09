# Progress Log

# 6/9/2022

Noctane is now at 6,700 lines of code. (There are approximatly 8,600 including blank and comment lines.)

The CPU module-level documentation is more complete than before, but documentation otherwise is still lacking.

## 6/2/2022

Noctane just hit 5,500 lines of code! (There are approximately 7,000 including blank and comment lines.)

In other news:
- CPU exceptions were implemented properly
- the CPU I-cache was fixed
- many CPU I/O registers were stubbed
- a tiny debugger was written to aid development
- the MMU was dropped as the PSX CPU doesn't have one

My current thoughts are to put GPU development on-hold for the moment and focus on cleaning up the CPU code. I would like to broadly and extensively expand documentation, both for others interested in reviewing the project as well as for myself.

## 5/28/2022

I've been working on Noctane nearly non-stop for about two weeks now. The CPU is mostly implemented, the GUI is partially implemented but currently unpolished, the CD-ROM drive is unfinished but nonetheless capable of generating a file hierarchy from an ISO image, and the GPU, GTE, and SPU are all wholly unimplemented. That being said, with the CEX-1000 KT-3 BIOS image that I've been testing with, Noctane makes it all the way to sending GP1 commands to the GPU before stalling. This will mark a shift in development from basic CPU functionality to the video hardware.

Although it is much too early to say, and I will have a busier schedule over the summer, I aim for Noctane to be capable of rendering the SCE logo in the BIOS in the next couple of weeks. After that, I would like to resume development on my other project, *boing*, to implement some needed GUI features for Noctane (i.e., tables, images).

Stay tuned!
