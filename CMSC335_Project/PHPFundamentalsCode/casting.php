<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			$value1 = 3.49;
			echo "Integer cast for $value1<br />";
			echo (integer)$value1;
			echo "<hr />";
			
			$value2 = 10.89;
			echo "Integer cast for $value2<br />";
			echo (integer)$value2;
			echo "<hr />";
			
			echo "Boolean cast for $value1<br />";
			echo (boolean)$value1;
			echo "<hr />";
			
			$value3 = 0;
			echo "Boolean cast for $value3<br />";
			echo "Result".(boolean)$value3."END";
			echo "<hr />";
			
			$length = "30in";
			echo "Integer cast for $length<br />";
			echo (integer)$length;
			echo "<hr />";
			
			$width = "5ft3in";
			echo "Integer cast for $width<br />";
			echo (integer)$width;
			echo "<hr />";
			
			$value4 = 200.50;
			echo "String cast for $value4<br />";
			echo (string)$value4;
			echo "<hr />";
		?>
   </body>
</html>