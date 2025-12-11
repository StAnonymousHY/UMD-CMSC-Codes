<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			echo "Maximum of 10/20 is ".maximum(10, 20)."<br />";
			echo "</p>";
		?>		
   </body>
</html>

<?php
	function maximum($x, $y) {
		if ($x > $y)
			return $x;
		return $y;
	}	
?>