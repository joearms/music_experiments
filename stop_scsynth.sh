#!/bin/bash

ps -ax | grep scsynth
killall scsynth
ps -ax | grep scsynth



