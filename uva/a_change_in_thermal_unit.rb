#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# A semi-literate-programming Ruby solution for the **A Change in Thermal Unit**
# problem from [UVa Online Judge's](http://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&category=84&page=show_problem&problem=3135) 
# problem set by [Brendan Hay](http://www.github.com/brendanhay). 

# ### Brief
# #### Background
# Measuring temperature and temperature differences are common task in many research and applications. 
# Unfortunately, there exists more than one unit of measuring temperatures. 
# This introduces a lot of confusion at times. 
# Two popular units of measurements are Celsius (`C`) and Fahrenheit (`F`). 
# The conversion of `F` from `C` is given by the formula:
#
#     F = 9/5 C + 32
#
# In this problem, you will be given an initial temperature in `C` and an increase in temperature in `F`.
# You would have to calculate the new temperature in `C`.
#
# #### Input
# Input starts with an integer `T (≤ 100)`, denoting the number of test cases.
# Each case contains a line with two integers `C` and `d (0 ≤ C, d ≤ 100)`, where `C` represents the initial temperature in Celsius and `d` represents the increase in temperature in Fahrenheit.
#
#     2
#     100 0
#     0 100
#
# #### Output
# For each case, print the case number and the new temperature in Celsius after rounding
# it to two digits after the decimal point.
#
#     Case 1: 100.00
#     Case 2: 55.56

 
