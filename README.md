# haskell-ping
Educational ping program implemented with Haskell

Program will send an ICMP (Internet Control Message Protocol) echo request to a specified interface on the network end, and then monitor a raw socket for an ICMP echo reply.

Helpful high level overview of ping here: http://images.globalknowledge.com/wwwimages/whitepaperpdf/WP_Mays_Ping.pdf

## Building a binary
 
Note: assumes you have downloaded and installed `stack`
```
stack build
```
The binary will be built and added to `haskell-ping/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/bin`

## Run the binary

Note: Must use `sudo` because a raw socket is being opened which requires admin priveledges

`stack exec` offers an easy way to execute the binary from the project dir without navigating to where the binary was put.
```
sudo stack exec hsping 127.0.0.1 --allow-different-user
```
You may of course also execute the binary directly.
```
cd .stack-work/install/x86_64-osx/lts-8.19/8.0.2/bin
sudo ./hsping 127.0.0.1
```

## ICMP Echo Request
From http://www.networksorcery.com/enp/protocol/icmp/msg8.htm#ICMP%20Header%20Checksum

<table>
  <tr>
    <td>00 01 02 03	04 05	06 07</td>
    <td>08 09 10 11 12 13 14 15</td>
    <td>16 17 18 19 20 21 22 23</td>
    <td>24 25 26 27 28 29 30 31</td>
  </tr>
  <tr>
    <td colspan="1">Type</td>
    <td colspan="1">Code</td>
    <td colspan="2">ICMP header checksum</td>
  </tr>
  <tr>
    <td colspan="2">Identifier</td>
    <td colspan="2">Sequence number</td>
  </tr>
  <tr>
    <td colspan="4">Data (variable length)</td>
  </tr>
</table>

**Type**. 8 bits. Set to 8.

**Code**. 8 bits. Cleared to 0.

**ICMP Header Checksum**. 16 bits.

From *RFC 792*: PAGE 13 OF (http://www.ietf.org/rfc/rfc792.txt)

*The checksum is the 16-bit ones's complement of the one's
complement sum of the ICMP message starting with the ICMP Type.
For computing the checksum , the checksum field should be zero.
If the total length is odd, the received data is padded with one
octet of zeros for computing the checksum.  This checksum may be
replaced in the future.*

**Identifier**. 16 bits.
This field is used to help match echo requests to the associated reply. It may be cleared to zero.

**Sequence number**. 16 bits.
This field is used to help match echo requests to the associated reply. It may be cleared to zero.

**Data**. Variable length.
Implementation specific data.
