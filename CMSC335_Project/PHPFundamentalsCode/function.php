<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			DISPLAYMULTTABLE(10);
			echo "<hr />";
			displayMultTable(5);
		?>		
   </body>
</html>

<?php
	function displayMultTable($limit) {
		echo "<table border=\"1\">";
		for ($outer = 1; $outer <= $limit; $outer++) {
			echo "<tr>";
			for ($inner = 1; $inner <= $limit; $inner++) {
				echo "<td>".($outer * $inner)."</td>";
			}
			echo "</tr>";
		}	
		echo "</table>";
	}
?>