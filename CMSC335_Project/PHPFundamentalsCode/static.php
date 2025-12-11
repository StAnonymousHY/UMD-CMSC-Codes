<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			echo "Product 10*20: ".product(10,20)."<br />";
			echo "Product 3*9: ".product(3,9)."<br />";
			echo "Product 5*7: ".product(5,7)."<br />";
			echo "</p>";
		?>		
   </body>
</html>

<?php
	function product($a, $b) {
		static $count = 0; // value of 0 assigned the first time is seen
		
		print("Count value: ".$count."<br />");
		$count++;
		
		return $a * $b;
	}
?>