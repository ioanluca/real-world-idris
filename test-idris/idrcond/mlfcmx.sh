#!/bin/sh

malfunction cmx -p tcpip.unix -p tcpip.udpv4-socket -p tcpip.tcpv4-socket -p tcpip.stack-socket -p tcpip -p nocrypto.lwt -p nocrypto -p mirage-unix -p mirage-types-lwt -p mirage-types -p mirage-runtime -p mirage-logs -p mirage-conduit -p mirage-clock-unix -p mirage-bootvar-unix -p lwt -p functoria-runtime -p cohttp-mirage idrcond.mlfa

