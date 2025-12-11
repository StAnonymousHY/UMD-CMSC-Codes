<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<p>";
			$val = 5;
			$fac = 1;
			$i = $val;
			do {
  				$fac *= $i;
  				$i--; 
			} while ($i > 0);
			echo "Factorial of $val is: $fac";
			echo "</p>";
		?>
   </body>
</html>