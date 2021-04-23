// This source code is subject to the terms of the Mozilla Public License 2.0 at https://mozilla.org/MPL/2.0/
// © systemalphatrader

//@version=4

study(title='CCI [SystemAlpha]', shorttitle='CCI [SA]', overlay = false)
len     = input(title="CCI Period", minval=1, defval=20)
src     = input(title="CCI Source", defval=close)
lbR     = input(title="Pivot Lookback Right", defval=4)
lbL     = input(title="Pivot Lookback Left", defval=2)
ob      = input(100, minval=0, title="Overbought")
os      = input(-100, minval=-200, title="Oversold")
obpbs   = input(200, minval=0, title="Overbought PBS")
ospbs   = input(-200, minval=-200, title="Oversold PBS")
pbs     = input(false, title="Point Break Scalp")
showsignals = input(true, title="Show Buy/Sell Signals")

divtype         = input(title="Divergence Type", defval='Regular', options=['Regular', 'Hidden', 'Both', 'None'])
plotBull        = divtype == 'Regular' or divtype == 'Both'
plotHiddenBull  = divtype == 'Hidden' or divtype == 'Both'
plotBear        = divtype == 'Regular' or divtype == 'Both'
plotHiddenBear  = divtype == 'Hidden' or divtype == 'Both'
showlabel       = input(true, title="Show Divergence Label")

showpivot   = input(false, title="Show Pivot Points")
chcut       = true //input(true, title = "Check Cut-Through!")
rangeUpper  = input(title="Max of Lookback Range", defval=60)
rangeLower  = input(title="Min of Lookback Range", defval=5)

bearColor   = color.red
bullColor   = color.green
hiddenBullColor = color.new(color.green, 50)
hiddenBearColor = color.new(color.red, 50)
textColor   = color.white
noneColor   = color.new(color.white, 100)

osc = cci(src, len)

plot(osc, title="CCI", linewidth=1, color=color.orange)
hline(0, title="Centerline", linestyle=hline.style_dashed,color=color.gray)

lineBuyBottom   = plot(os, color=color.gray, title="Oversold Level")
lineSellTop     = plot(ob, color=color.gray, title="Overbought Level")

lineBuy     = plot(series=(osc < os ? osc : os), transp=100, title="n/a", editable=false)
lineSell    = plot(series=(osc > ob ? osc : ob), transp=100, title="n/a", editable=false)

fill(lineSellTop, lineBuyBottom, title="Background", color=color.gray, transp=80)
fill(plot1=lineBuy, plot2=lineBuyBottom, color=color.red, transp=50, title="Oversold Area")
fill(plot1=lineSellTop, plot2=lineSell, color=color.green, transp=50, title="Overbought Area")

//Point Break Scalp
hline(pbs? obpbs : na, title="Overbought PBS", linestyle=hline.style_dashed, editable = false)
hline(pbs? ospbs: na, title="Oversold PBS", linestyle=hline.style_dashed, editable = false)

//Divergence
float top = na
float bot = na
top := pivothigh(osc, lbL, lbR)
bot := pivotlow(osc, lbL, lbR)

phFound = na(top) ? false : true
plFound = na(bot) ? false : true

_inRange(cond) =>
    bars = barssince(cond == true)
    rangeLower <= bars and bars <= rangeUpper

alertcondition(osc[1] > 100.0 and osc[2] < 100.0, title="CCI value crosses over 100.0", message="Check charts for a CCI cross over 100.0")
alertcondition(osc[1] < 100.0 and osc[2] > 100.0, title="CCI value crosses under 100.0", message="Check charts for a CCI cross under 100.0")
alertcondition(osc[1] > -100. and osc[2] < -100.0, title="CCI value crosses over -100.0", message="Check charts for a CCI cross over -100.0")
alertcondition(osc[1] < -100.0 and osc[2] > -100.0, title="CCI value crosses under -100.0", message="Check charts for a CCI cross under -100.0")

//No Cut cut-through
topc = 0, botc = 0
topc := top ? lbL : nz(topc[1]) + 1
botc := bot ? lbL : nz(botc[1]) + 1

// check cut-through in indicators
nocut1(indi, len)=>
    _ret = true
    diff = (indi - nz(indi[len])) / len
    ln = indi - diff
    for x = 1 to len -1
        if nz(indi[x]) > ln
            _ret := false
            break
        ln := ln - diff
    _ret

topok = nocut1(osc, topc)

// check cut-through in indicators
nocut2(indi, len)=>
    _ret = true
    diff = (indi - nz(indi[len])) / len
    ln = indi - diff
    for x = 1 to len -1
        if nz(indi[x]) < ln
            _ret := false
            break
        ln := ln - diff
    _ret

botok = nocut2(osc, botc)

//Show Pivot

plotshape(showpivot and phFound and (not chcut or topok) ? osc[lbR] : na,title="Pivot High",text="[PH]",style=shape.labeldown,color=color.white,textcolor=color.black,location=location.absolute,transp=0,offset = -lbR, editable = false)
plotshape(showpivot and plFound and (not chcut or botok) ? osc[lbR] : na,title="Pivot Low",text="[PL]",style=shape.labeldown,color=color.white,textcolor=color.black,location=location.absolute,transp=0,offset = -lbR, editable = false)

//------------------------------------------------------------------------------
// Regular Bullish

// Osc: Higher Low
oscHL = osc[lbR] > valuewhen(plFound, osc[lbR], 1) and _inRange(plFound[1])

// Price: Lower Low
priceLL = low[lbR] < valuewhen(plFound, low[lbR], 1)

bullCond = plotBull and priceLL and oscHL and plFound

plot(
	 plFound ? osc[lbR] : na,
	 offset=-lbR,
	 title="Regular Bullish",
	 linewidth=1,
	 color=(bullCond ? bullColor : noneColor),
	 transp=0, 
	 editable = false)

plotshape(
	 bullCond and showlabel ? osc[lbR] : na,
	 offset=-lbR,
	 title="Regular Bullish Label",
	 text="R",
	 style=shape.labelup,
	 location=location.absolute,
	 color=bullColor,
	 textcolor=textColor,
	 transp=0, 
	 editable = false)

alertcondition(bullCond, title="Regular bullish divergence in CCI found", message="Check charts for a regular bullish divergence found with CCI")

//------------------------------------------------------------------------------
// Hidden Bullish

// Osc: Lower Low
oscLL = osc[lbR] < valuewhen(plFound, osc[lbR], 1) and _inRange(plFound[1])

// Price: Higher Low
priceHL = low[lbR] > valuewhen(plFound, low[lbR], 1)

hiddenBullCond = plotHiddenBull and priceHL and oscLL and plFound

plot(
	 plFound ? osc[lbR] : na,
	 offset=-lbR,
	 title="Hidden Bullish",
	 linewidth=1,
	 color=(hiddenBullCond ? hiddenBullColor : noneColor),
	 transp=50, 
	 editable = false)
	 
plotshape(
	 hiddenBullCond and showlabel ? osc[lbR] : na,
	 offset=-lbR,
	 title="Hidden Bullish Label",
	 text="H",
	 style=shape.labelup,
	 location=location.absolute,
	 color=bullColor,
	 textcolor=textColor,
	 transp=50, 
	 editable = false)

alertcondition(hiddenBullCond, title="Hidden bullish divergence in CCI found", message="Check charts for a hidden bullish divergence found with CCI")

//------------------------------------------------------------------------------
// Regular Bearish

// Osc: Lower High
oscLH = osc[lbR] < valuewhen(phFound, osc[lbR], 1) and _inRange(phFound[1])

// Price: Higher High
priceHH = high[lbR] > valuewhen(phFound, high[lbR], 1)

bearCond = plotBear and priceHH and oscLH and phFound

plot(
	 phFound ? osc[lbR] : na,
	 offset=-lbR,
	 title="Regular Bearish",
	 linewidth=1,
	 color=(bearCond ? bearColor : noneColor),
	 transp=0, 
	 editable = false)

plotshape(
	 bearCond and showlabel ? osc[lbR] : na,
	 offset=-lbR,
	 title="Regular Bearish Label",
	 text="R",
	 style=shape.labeldown,
	 location=location.absolute,
	 color=bearColor,
	 textcolor=textColor,
	 transp=0, 
	 editable = false)

alertcondition(bearCond, title="Regular bearish divergence in CCI found", message="Check charts for a regular bearish divergence found with CCI")

//------------------------------------------------------------------------------
// Hidden Bearish

// Osc: Higher High
oscHH = osc[lbR] > valuewhen(phFound, osc[lbR], 1) and _inRange(phFound[1])

// Price: Lower High
priceLH = high[lbR] < valuewhen(phFound, high[lbR], 1)

hiddenBearCond = plotHiddenBear and priceLH and oscHH and phFound

plot(
	 phFound ? osc[lbR] : na,
	 offset=-lbR,
	 title="Hidden Bearish",
	 linewidth=1,
	 color=(hiddenBearCond ? hiddenBearColor : noneColor),
	 transp=50, 
	 editable = false)

plotshape(
	 hiddenBearCond and showlabel ? osc[lbR] : na,
	 offset=-lbR,
	 title="Hidden Bearish Label",
	 text="H",
	 style=shape.labeldown,
	 location=location.absolute,
	 color=bearColor,
	 textcolor=textColor,
	 transp=50, 
	 editable = false)

alertcondition(hiddenBearCond, title="Hidden bearish divergence in CCI found", message="Check charts for a hidden bearish divergence found with CCI")

// Buy/Sell Signals

// use curvature information to filter out some false positives
sellsignals = pbs? showsignals and phFound and osc[lbR] and osc[lbR] > obpbs: showsignals and phFound and osc[lbR] and osc[lbR] > ob
buysignals = pbs? showsignals and plFound and osc[lbR] and osc[lbR] < ospbs: showsignals and plFound and osc[lbR] and osc[lbR] < os

plot(buysignals ? osc[lbR]: na, color = color.white , style = plot.style_circles, linewidth = 3, title="Buy Signal Outline", offset=-lbR)
plot(buysignals ? osc[lbR]: na, style=plot.style_circles, color= color.green, linewidth=2, title="Buy Signal", transp=0, offset=-lbR)
plot(sellsignals ? osc[lbR]: na, color = color.white , style = plot.style_circles, linewidth = 3, title="Sell Signal Outline", offset=-lbR)
plot(sellsignals ? osc[lbR]: na, style=plot.style_circles, color= color.red, linewidth=2, title="Sell Signal", transp=0, offset=-lbR)