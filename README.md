The Algorithm
====

This repository contains an implementation of the LZ78 compression
algorithm.  [Here](https://en.wikipedia.org/wiki/LZ77_and_LZ78) is a description of the algorithm.

This implementation uses a 16-bit index, yielding a dictionary of 65,535
entries.  The dictionary is prefilled with 256 entries, one for each
possible value for a byte.  The dictionary is reset to the prefilled
dictionary when the index overfills.

The compressed file always ends with an index and a character, even when
the end of the file could have been encoded with just an index.  This
simplifies the compression and decompression processes.


Implementation
====

The compression process is as follows: the decompressed file is
read and unpacked as a list of Word8, the list is passed to the compressString
function, which returns a list of entries.  Each entry consists of an index
and a character.  That list of entries is written to the output file.

Here is how decompression works: the compressed file is read as a ByteString,
the ByteString is parsed into a list of entries, and the list of entries is
passed to the decompressString function, which returns a list of Word8.
That list is packed into a ByteString, which is written to the output file.

The Lazy ByteString library is used for IO and the Binary.Get and Binary.Put
libraries are used to parse and write structures to binary files.


Issues
=====

There are a number of issues with the program:

- The program can run out of memory for when decompressing relatively small
files.  On my computer, I was able to decompress a 1-MB file without
difficulty but a 10-MB file required extra stack space.  It seems that the
code for decompression does not take advantage of lazy bytestrings.

- Although the program works correctly for plain text files, it corrupts
binary files.  The decompressed file is somewhat different from the
original.  I have not identified the cause of the issue.

