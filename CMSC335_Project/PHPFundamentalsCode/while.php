<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			echo "<h1>Power Table</h1>";
			$i = 1;
			$limit = 100;
			while ($i <= $limit) {
				print($i."  ".($i * $i)); # What if we remove \n
				echo "<br />";
				$i++;
			}
		?>
   </body>
</html>