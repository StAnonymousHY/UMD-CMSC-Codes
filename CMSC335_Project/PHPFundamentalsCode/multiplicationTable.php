<!doctype html>
<html>
    <head> 
        <meta charset="utf-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			$limit = 10;
			echo "<table border=\"1\">";  // Notice the use of escape for "
				for ($outer = 1; $outer <= $limit; $outer++) {
					echo "<tr>";
					for ($inner = 1; $inner <= $limit; $inner++) {
						echo "<td>".($outer * $inner)."</td>";
					}				
					echo "</tr>";
				}
			echo "</table>";
		?>
   </body>
</html>