/* Copyright (c) 2010,2011 Alessandro Sivieri <sivieri@elet.polimi.it>
 * 
 * This file is part of CREST-Erlang.
 * 
 * CREST-Erlang is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * CREST-Erlang is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
 */ 

// jQuery things
$(document).ready(function(){
	createTableInstalled();
	createTableLocal();

	$("#refreshform").submit(function(){
		createTableInstalled();
		createTableLocal();
	});
	$("#localtable a").bind("click", function(event){
		$.get($(this).attr("href"), {}, function(response){
			$("#newlocal").html(response.key);
		});
		return false;

	});
});

function createTableInstalled()
{
	$.ajax({
		url:"crest/manager/installed",
		type:"GET",
		timeout:10000,
		success: function(data) {
			$("#processestable").dataTable({
				aaData:data.aaData,
				bProcessing:true,
				bDestroy:true
			});
		},
		error: function(data, error) {
			alert("Error: " + error);
		}
	});
}

function createTableLocal()
{
	$.ajax({
		url:"crest/manager/local",
		type:"GET",
		timeout:10000,
		success: function(data) {
			$("#localtable").dataTable({
				aaData:data.aaData,
				bProcessing:true,
				bDestroy:true
			});
		},
		error: function(data, error) {
			alert("Error: " + error);
		}
	});
}

function updateModuleFunction(val)
{
    var elem = document.getElementById("funname");
    var fname = basename(val);
    elem.value = fname + ":" + fname;
}

function basename(str)
{
   var base = new String(str).substring(str.lastIndexOf('\\') + 1); 
   if(base.lastIndexOf(".") != -1)       
       base = base.substring(0, base.lastIndexOf("."));
   return base;
}
