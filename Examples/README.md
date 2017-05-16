<p>First of all, download nhlplayers.csv, and if you're using any of my code, make sure that the file path in the first line is correct. When you open the .csv file, click "Raw" which should give you the plain text of the comma separated values. In Google Chrome, go to Save Page As... in your browser options. Then, you can then save that page as a .csv file in your working directory.<p>
<p>gini.py has Python code and lorenz.R has R code to calculate Gini coefficients for each team. Using those files is as simple as copying and pasting the lines of code into RStudio or whatever Python environment you use. The R code is structured in a way that makes it more conducive to graphing individual Lorenz curves, and that's mostly because I think graphs are prettier in R. Also, if you don't want to code at all, you can mess around with the Excel spreadsheet (lorenz.xlsx), but you'd have to manually paste in any new data.<p>
<p>Let me try to anticipate some questions/concerns:<p>
<p><i>Your code has a mistake in it!</i><br>
Absolutely not, it's been cross-referenced with multiple Gini coefficient calculators.</p>
<p><i>Your code is inefficient!</i><br>
This is probably true, I am certainly not a master of coding. If you find a much simpler way of expressing something, feel free to let me know.</p>
<p><i>Your code isn't working for me!</i><br>
Check to see if the file path in the first line is correct, you'll want to read in the data from the right place in your computer. You may also have trouble if you're running an older version of Python.</p>
<p><i>You should exclude defensemen/include more than 21 skaters/etc.!</i><br>
Yes, you're right! Please modify my code to analyze anything in any way that you want, that's the whole point.</p>
