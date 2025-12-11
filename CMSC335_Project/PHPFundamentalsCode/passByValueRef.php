<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			$x = 100;
			$y = 200;
			print("Before swap: \$x=".$x." \$y=".$y);
			print("<br />");
			swap($x,$y);
			print("After swap: \$x=".$x." \$y=".$y);
			echo "</p>";
		?>		
   </body>
</html>

<?php
	function swap(&$a, &$b) {
		echo "On entry to swap $a $b <br />";
		$temp = $a;
		$a = $b;
		$b = $temp;
    }
?>