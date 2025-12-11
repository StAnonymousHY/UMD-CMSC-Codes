<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			// declaring some variables
			$age = 10;
			$salary = 20.50;
			$water_temperature = 90.0;
			
			echo "<p>";
			echo "Age is: $age, ";
			print("Salary is: $salary, ");
			print("Water temperature is: $water_temperature<br />");
			
			$salary = "A whole lot of money";
			print ("<em>{$salary}</em><br />");
			
			// Illustrates the difference between single/double quotes
			print ('<em>$salary</em>');
			echo "</p>";
		?>
   </body>
</html>