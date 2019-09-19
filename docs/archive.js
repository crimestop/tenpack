$(document).ready(function(){
	$("#version-selector").change(function(){
		var version=$("#version-selector").find("option:selected").text();
		var url="resources/lib/Tensor-"+version+".zip";
		$("#version-url").attr("href",url)
		//alert($("#version-url").attr("href"));
	});
});