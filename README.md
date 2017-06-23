# haskell-ping
Educational ping program implemented with Haskell

Program will send an ICMP (Internet Control Message Protocol) echo request to a specified interface on the network end, and then monitor a raw socket for an ICMP echo reply.

Helpful high level overview of ping here: http://images.globalknowledge.com/wwwimages/whitepaperpdf/WP_Mays_Ping.pdf

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
The 16-bit one's complement of the one's complement sum of the ICMP message, starting with the ICMP Type field. When the checksum is computed, the checksum field should first be cleared to 0. When the data packet is transmitted, the checksum is computed and inserted into this field. When the data packet is received, the checksum is again computed and verified against the checksum field. If the two checksums do not match then an error has occurred.

**Identifier**. 16 bits.
This field is used to help match echo requests to the associated reply. It may be cleared to zero.

**Sequence number**. 16 bits.
This field is used to help match echo requests to the associated reply. It may be cleared to zero.

**Data**. Variable length.
Implementation specific data.
