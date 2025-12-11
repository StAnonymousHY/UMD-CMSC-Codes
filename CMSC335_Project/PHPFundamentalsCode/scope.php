<?php
	$myGlobal = 200;
	
	function scopeCheck() {
   		$localVar = 10;
   		print("In scopeCheck: ".$localVar."<br />");
	}

	function scopeCheckTwo() {
		print("In scopeCheckTwo: ".$myGlobal."<br />");
	}

	function scopeCheckThree() {
   		global $myGlobal;  // provides access to the global
   		print("In scopeCheckThree: ".$myGlobal."<br />");
	}
	
	function scopeCheckFour() {
		global $secondGlobal;  // global declared below
		print("In scopeCheckFour: ".$secondGlobal."<br />");
	}
	
	$secondGlobal = 400;
?>

<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			scopeCheck();
			
			print("Outside scopeCheck: ".$localVar."<br />");
			echo "<hr />";

			scopeCheckTwo();
			echo "<hr />";

			scopeCheckThree();
			echo "<hr />";
			
			scopeCheckFour();
			echo "<hr />";
			
			echo "</p>";
		?>		
   </body>
</html>