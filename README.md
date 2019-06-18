# FAO-Figures

Fig. 1:
- similar to the old “BdivBmsy Props…” figure that we used to produce, attached
- 3 new categories to use instead: BdivBmsy < 0.8 (red), between 0.8-1.2 (yellow), and >1.2 (green)
- don’t need for UdivUmsy or CdivMSY
- 1 page per FAOarea, could probably be about full page width and half-page height
- instead of solid colors for each of 3 bands, scale them so they range from full saturation for years of 100% coverage of B/BMSY estimates, and no saturation (white) for years of 0% coverage. Could be done by using alpha parameter of rgb() that depends on fraction of stocks covered in any given year, but there could be other ways as well. 
- x-axis = “Year”, y-axis = “Proportion of stocks in B/BMSY category”, plot title = FAOname
- legend 1 = “Stock status:”, with categories B/BMSY < 0.8, 0.8 < B/BMSY < 1.2, 1.2 < B/BMSY
- legend 2 = “Coverage:”, with 3 horizontal bands ranging from white to full saturation (R/Y/G), and under or over the 3 bands, a few markers such as “0%, 25%, 50%... 100%”. See the 3-panel state-space model plots that you’ve put on the website by region or taxgroup as an example, now there would be 3 bands instead of 1.

2 panels for each region: all stocks weighted equally, and stocks weighted by MSY or mean catch over time series (side by side preferably)

Fig. 2:
- Total landings time series covered by stocks in RAM database and in FAO landings database, separated by FAO area
- we could possibly fit all regions in something like a 5 x 4 panel page, with tick mark labels and axis titles only in the outer margins, or else maybe spread over 2 pages is all on one page is too crowded
- I’ll send you FAO landings data to use.
- don’t need the “RAM 2009” line, and don’t need labels in every panel, maybe just one panel (color-coded), and use the same color coding for remaining lines. 
- the timeseries_values_views table will probably be sufficent for the RAM data. There is a chance that some older portions of the time series may be missed for some stocks if the more recent assessments 

other figs (for after Figs 1 and 2 above):
Similar to the following figures you’ve already made for the website, but now separated by FAOarea (instead of region or taxgroup)
- Biomass Coverage (all stocks combined), both types
- Added productivity time series, combine 4 panels onto one page if possible, maybe half -  ¾ page
- Added productivity vs. biomass, combine 4 panels onto one page if possible, maybe half -  ¾ page


For Fig. 1 and the updates of existing figures, we only need to cover the FAOareas that have some representation in RAM (and we might not bother with FAOareas that only have one or a few RAM stocks).

For Fig. 2, however, it will be useful to show our data gaps by covering all (marine) regions even if there are no RAM stocks represented. How about we do a 5 x 4 layout, with one blank panel for a legend, and see what that looks like.
There is a modification I just thought of (feel free to add to protocol file):
- FAO landings lists its salmon in marine regions, whereas we've listed the freshwater region as primary region. Just for this Fig. 2, could you change the FAOarea designations for all Pacific salmon stocks in RAM to their corresponding marine region? The salmon stocks can be identified through their "region" labels, as "...(salmon)". If they're currently listed as area 02, those could be changed to area 67, and if any are from area 04 those could be changed to 61.
