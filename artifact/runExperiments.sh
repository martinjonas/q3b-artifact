#!/bin/bash

TIMEOUT=1200
if [ -n "$1" ]; then
  TIMEOUT=$1
fi

MEMLIMIT=8
if [ -n "$2" ]; then
  MEMLIMIT=$2
fi

CORES=`cat /proc/cpuinfo | grep -c 'core id'`
RAM=`free -g | grep "Mem:" | awk '{print $2}'`

getThreads()
{
  TOOL_CORES=$1
  MAX_CORES=$(expr $CORES / $TOOL_CORES)
  MAX_RAM=$(expr $RAM / $MEMLIMIT)
  if [ "$MAX_CORES" -gt "$MAX_RAM" ]; then
    echo $MAX_RAM
  else
    echo $MAX_CORES
  fi
}

echo "The computer has $CORES cores and $RAM GiB of free RAM"

cd experiments/
rm -rf results
export PYTHONPATH=/home/cav/q3b-artifact/artifact/experiments/:$PYTHONPATH

echo "Running $(getThreads 2) parallel instances of Boolector"
benchexec boolector.xml -N `getThreads 2` -T "$TIMEOUT"s -M "$MEMLIMIT"GB

echo "Running $(getThreads 1) parallel instances of CVC4"
benchexec cvc4.xml -N `getThreads 1` -T "$TIMEOUT"s -M "$MEMLIMIT"GB

echo "Running $(getThreads 3) parallel instances of Q3B"
benchexec q3b.xml -N `getThreads 3` -T "$TIMEOUT"s -M "$MEMLIMIT"GB

echo "Running $(getThreads 1) parallel instances of Z3"
benchexec z3.xml -N `getThreads 1` -T "$TIMEOUT"s -M "$MEMLIMIT"GB
cd ..
