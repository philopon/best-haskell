'use strict';

angular.module("bestHaskellApp", ['ngRoute']) // {{{
.config(function($routeProvider){
  $routeProvider
    .when('/', {
      templateUrl: 'view/index.html',
      controller:  'IndexController'
    })
    .when('/package/:package', {
      templateUrl: 'view/package.html',
      controller:  'PackageController'
    })
    .when('/category/:category*', {
      templateUrl: 'view/category.html',
      controller:  'CategoryController'
    });
}) // }}}
.directive('rankingTable', function(){ // {{{
  return {
    restrict: 'E',
    replace: true,
    templateUrl: 'view/rankingTable.html',
    scope: {caption: '@', ranking: '='},
    link: function($scope){
      $scope.$watch('ranking', function(newVal){
        if(!newVal) {return;}
        $scope.items = $scope.ranking;
      });
    }
  }
}) // }}}
.directive('downloadsChart', function(){ // {{{
  return {
    restrict: 'A',
    scope: {downloads: '=', height: '@'},
    link: function($scope, $elems){
      function getScopeSize() {
        var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
        if      (width >= 1200) {width = 1170}
        else if (width >=  992) {width =  970}
        else if (width >=  768) {width =  750}
        width -= 50;

        return {width: width, height: width * 9/16};
      }
      var size = getScopeSize();

      var dateBisector   = d3.bisector(function(d){return d.date;}).left;
      var dateFormatter  = d3.time.format('%b %d, %Y');
      var countFormatter = d3.format(',d');

      var margin = {top: 20, right: 80, bottom: 80, left: 80};
      $elems.addClass('downloads-chart');

      var height = size.height - margin.top  - margin.bottom;
      var width  = size.width  - margin.left - margin.right;

      var svg = d3.select($elems[0]).append('svg')
          .attr('width',  size.width)
          .attr('height', size.height);

      var plotarea = svg.append('g')
          .attr('transform', "translate(" + margin.left + "," + margin.top + ")");

      var x  = d3.time.scale().range([0, width]);
      var yt = d3.time.scale().range([height, 0]);
      var yc = d3.time.scale().range([height, 0]);

      var xAxis  = d3.svg.axis().scale(x).orient('bottom');
      var yAxisT = d3.svg.axis().scale(yt).orient('left').ticks(10).tickFormat(countFormatter);
      var yAxisC = d3.svg.axis().scale(yc).orient('right').ticks(10).tickFormat(countFormatter);

      var total = d3.svg.line()
        .x(function(d){return x(d.date);})
        .y(function(d){return yt(d.total);});

      var count = d3.svg.line()
        .x(function(d){return x(d.date);})
        .y(function(d){return yc(d.count);});

      var countPath = plotarea.append('path').attr('class', 'line count');
      var totalPath = plotarea.append('path').attr('class', 'line total');

      var cursor = plotarea.append('line')
        .attr('class', 'cursor line')
        .attr('x1',   0)
        .attr('y1',   0)
        .attr('x2',   0)
        .attr('y2', height)
        .attr('display', 'none');

      var inspector = plotarea.append('g')
        .attr('class', 'inspector')
        .attr('display', 'none');

      inspector.append('rect')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width',  100)
        .attr('height',  50);

      var inspectorDate = inspector.append('text')
        .attr('class', 'date')
        .attr('text-anchor', 'end')
        .attr('x', 95)
        .attr('y', 15);

      inspector.append('text')
        .attr('x', 3)
        .attr('y', 30)
        .text('count:');

      inspector.append('text')
        .attr('x', 3)
        .attr('y', 45)
        .text('total:');

      var inspectorCount = inspector.append('text')
        .attr('class', 'count')
        .attr('x', 95)
        .attr('y', 30)
        .style('text-anchor', 'end');

      var inspectorTotal = inspector.append('text')
        .attr('class', 'count')
        .attr('x', 95)
        .attr('y', 45)
        .style('text-anchor', 'end');

      function updateWindow () {
        var size = getScopeSize();
        var width  = size.width  - margin.left - margin.right;
        var height = size.height - margin.top  - margin.bottom;
        svg.attr('width', size.width).attr('height', size.height);
        x.range([0, width]);
        yt.range([height, 0]);
        yc.range([height, 0]);

        cursor.attr('y2', height);

        svg.select('.x.axis').call(xAxis)
            .attr('transform', 'translate(0,' + height + ')')
          .selectAll('text')
            .attr('transform', 'rotate(90)')
            .attr('x', 8)
            .attr('dy', '-0.5em')
            .style('text-anchor', 'start');

        svg.select('.y.axis.left').call(yAxisT);
        svg.select('.y.axis.right').call(yAxisC);

        svg.select('.y.axis.right')
            .attr('transform', 'translate(' + width + ',0)')
            .call(yAxisC);

        svg.select('.line.count').attr('d', count);
        svg.select('.line.total').attr('d', total);
      }
      window.onresize = updateWindow;

      var cursor_initialized = false;
      function cursorVisible (vis) {
        if (vis && cursor_initialized) { cursor.attr('display', null);   inspector.attr('display', null); } 
        else     { cursor.attr('display', 'none'); inspector.attr('display', 'none'); }
      }

      function mouseMove () {
        var mouse = d3.mouse(this);
        var mx    = mouse[0] - margin.left;

        var size = getScopeSize();
        var width  = size.width  - margin.left - margin.right;
        var height = size.height - margin.top  - margin.bottom;
        if (mx < 0 || mx > width || mouse[1] > height + margin.top || mouse[1] < margin.top) { cursorVisible(false); return }

        cursorVisible(true)

        var x0    = x.invert(mouse[0] - margin.left);
        var i     = dateBisector($scope.downloads, x0, 1);
        var d0    = $scope.downloads[i];
        var d1    = $scope.downloads[i+1];
        var data  = d1 && x0 - d0.date > d1.date - x0 ? d1 : d0;

        var tx = mouse[0] > width  + margin.left - 100 ? mx - 100 : mx;
        var ty = mouse[1] > height + margin.top  -  50 ? mouse[1] - margin.top - 50 : mouse[1] - margin.top;
        inspector.attr('transform', 'translate(' + tx + ',' + ty + ')');
        
        inspectorDate.text(dateFormatter(data.date));
        inspectorCount.text(countFormatter(data.count));
        inspectorTotal.text(countFormatter(data.total));
        cursor.attr('transform', 'translate(' + mx + ',0)');
        cursor_initialized = true;
      }

      $scope.$watch('downloads', function(newVal, oldVal){
        if(!newVal) {return;}

        x.domain(d3.extent($scope.downloads, function(d){return d.date;}));
        yt.domain([0, d3.extent($scope.downloads, function(d){return d.total;})[1]]);
        yc.domain([0, d3.extent($scope.downloads, function(d){return d.count;})[1]]);

        countPath.datum($scope.downloads);
        totalPath.datum($scope.downloads);

        plotarea.append('g')
            .attr('class', 'x axis');

        plotarea.append('g')
            .attr('class', 'y axis left')
          .append('text')
            .attr('class', 'axis-label')
            .attr('transform', 'rotate(-90)')
            .attr('y', 12)
            .attr('text-anchor', 'end')
            .text('total download');

        plotarea.append('g')
            .attr('class', 'y axis right')
          .append('text')
            .attr('class', 'axis-label')
            .attr('transform', 'rotate(-90)')
            .attr('y', -4)
            .attr('text-anchor', 'end')
            .text('daily download');

        svg
          .on('mouseover', function(){cursorVisible(true)})
          .on('mouseout',  function(){cursorVisible(false)})
          .on('mousemove', mouseMove);

        updateWindow();

      });
    }
  }
}) // }}}
.controller("IndexController", function($rootScope, $scope, $http){ // {{{
  $rootScope.title     = "index";
  $http.get('/').success(function(data){
    $scope.nPackages  = data.nPackages;
    $scope.lastUpdate = data.lastUpdate;
    $scope.total      = data.total.ranking;
    $scope.weekly     = data.weekly.ranking;
    $scope.monthly    = data.monthly.ranking;
  });
}) // }}}
.controller("CategoryController", function($rootScope, $scope, $routeParams, $http){ // {{{
  var cat = $routeParams.category;
  $scope.category = cat;
  $rootScope.title = "Category:" + cat;
  $http({method: "GET", url: '/ranking', params: {category: cat}})
    .success(function(data){
      $scope.ranking = data.ranking;
    });
}) // }}}
.controller('PackageController', function($rootScope, $scope, $routeParams, $http){ // {{{
  $http.get('/package/' + $routeParams.package).success(function(data){
    for(var i = 0; i < data.downloads.length; i++){
      data.downloads[i].date  = new Date(data.downloads[i].date);
    }
    $scope.error          = null;
    $scope.complete       = true;
    $scope.author         = data.author;
    $scope.bugReports     = data.bugReports;
    $scope.category       = data.category;
    $scope.copyright      = data.copyright;
    $scope.description    = data.description;
    $scope.downloads      = data.downloads;
    $scope.executables    = data.executables;
    $scope.hasLibrary     = data.hasLibrary;
    $scope.homepage       = data.homepage;
    $scope.initialRelease = new Date(data.initialRelease);
    $scope.license        = data.license;
    $scope.maintainer     = data.maintainer;
    $scope.name           = data.name;
    $rootScope.title      = data.name;
    $scope.packageUrl     = data.packageUrl;
    $scope.stability      = data.stability;
    $scope.synopsis       = data.synopsis;
    $scope.total          = data.total;
    $scope.version        = data.version;
  })
  .error(function(data, status){
    $scope.complete = true;
    $scope.error = {
      title: status,
      description: data
    }
  });
}) // }}}
