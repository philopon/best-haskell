'use strict';

angular.module("bestHaskellApp", ['ngRoute', 'angulartics', 'angulartics.google.analytics']) // {{{
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
    })
    .when('/categories', {
      templateUrl: 'view/categories.html',
      controller: 'CategoriesController'
    })
    .when('/search/:query*', {
      templateUrl: 'view/search.html',
      controller:  'SearchController'
    })
    .otherwise({ redirectTo: '/' });
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
        var svgwidth = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
        if      (svgwidth >= 1200) {svgwidth = 1170}
        else if (svgwidth >=  992) {svgwidth =  970}
        else if (svgwidth >=  768) {svgwidth =  750}
        svgwidth -= 20;
        var width = svgwidth >= 768 ? svgwidth : 730;
        var scale = svgwidth / width;
        return {width: width, height: width * 9/16, svgwidth: svgwidth, svgheight: svgwidth * 9/16, scale: scale};
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
          .attr('width',  size.svgwidth)
          .attr('height', size.svgheight);

      var plotarea = svg.append('g')
          .attr('transform', "scale(" + size.scale + ',' + size.scale + ') translate(' + margin.left + "," + margin.top + ")" );

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
        svg.attr('width', size.svgwidth).attr('height', size.svgheight);
        plotarea.attr('transform', "scale(" + size.scale + ',' + size.scale + ') translate(' + margin.left + "," + margin.top + ")" );
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
      $(window).on('orientationchange resize', updateWindow);

      var cursor_initialized = false;
      function cursorVisible (vis) {
        if (vis && cursor_initialized) { cursor.attr('display', null);   inspector.attr('display', null); } 
        else     { cursor.attr('display', 'none'); inspector.attr('display', 'none'); }
      }

      function mouseMove () {
        var size = getScopeSize();
        var mouse = d3.mouse(this);
        mouse = [mouse[0] / size.scale, mouse[1] / size.scale];
        var mx    = mouse[0] - margin.left;

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
    $scope.complete   = true;
    $scope.nPackages  = data.nPackages;
    $scope.lastUpdate = data.lastUpdate;
    $scope.total      = data.total.ranking;
    $scope.weekly     = data.weekly.ranking;
    $scope.new        = data.new.ranking;
  }).error(function(data, status){
    $scope.complete = true;
    $scope.error = {
      title: status,
      description: data
    }
  });
}) // }}}
.controller("CategoryController", function($rootScope, $scope, $routeParams, $http){ // {{{
  var cat = $routeParams.category;
  $scope.category = cat;
  $rootScope.title = "Category:" + cat;
  $http({method: "GET", url: '/', params: {category: cat}})
    .success(function(data){
      $scope.complete   = true;
      $scope.nPackages  = data.nPackages;
      $scope.lastUpdate = data.lastUpdate;
      $scope.total      = data.total.ranking;
      $scope.weekly     = data.weekly.ranking;
      $scope.new        = data.new.ranking;
    }).error(function(data, status){
      $scope.complete = true;
      $scope.error = {
        title: status,
        description: data
      }
    });
}) // }}}
.controller('PackageController', function($rootScope, $scope, $routeParams, $http){ // {{{
  $scope.name = $routeParams.package;
  $http.get('/package/' + $routeParams.package).success(function(data){
    for(var i = 0; i < data.downloads.length; i++){
      data.downloads[i].date  = new Date(data.downloads[i].date);
    }
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
.controller('CategoriesController', function($rootScope, $scope, $http){ // {{{
  $rootScope.title = "categories";
  $http.get('/categories').success(function(data){
    $scope.complete   = true;
    $scope.categories = data.categories;
    var max = 0;
    for(var i = 0; i < data.categories.length; i++){
      max = Math.max(max, data.categories[i].count);
    }
    $scope.fsFun = function(c) {
      return 14 + Math.log(c) / Math.log(max) * 36;
    }
  })
  .error(function(data, status){
    $scope.complete = true;
    $scope.error = {
      title: status,
      description: data
    }
  });
}) // }}}
.controller('NavbarController', function($scope, $location){ // {{{
  $scope.submit = function(){
    $location.path('/search/' + $scope.query);
  };
}) // }}}
.controller('SearchController', function($rootScope, $routeParams, $scope, $http){ // {{{
  $rootScope.title = "Search:" + $routeParams.query;
  $scope.query = $routeParams.query;
  $http({method: 'GET', url: '/ranking', params: {q: $scope.query, limit: 100}}).success(function(data){
    $scope.complete = true;
    $scope.result = data.ranking;
  })
  .error(function(data, status){
    $scope.complete = true;
    $scope.error = {
      title: status,
      description: data
    }
  });
}); // }}}
