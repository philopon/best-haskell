'use strict';

angular.module("bestHaskellApp", ['ngRoute', 'angulartics', 'angulartics.google.analytics']) // {{{
.config(function($routeProvider, $analyticsProvider){
  $routeProvider
    .when('/', {
      templateUrl: 'view/index.html',
      controller:  'IndexController'
    })
    .when('/package/:package', {
      templateUrl: 'view/package.html',
      controller:  'PackageController'
    })
    .when('/search/:query*', {
      templateUrl: 'view/search.html',
      controller:  'SearchController',
      reloadOnSearch: false
    })
    .when('/detail/:mode', {
      templateUrl: 'view/detail.html',
      controller:  'DetailController',
      reloadOnSearch: false
    })
    .when('/categories', {
      templateUrl: 'view/categories.html',
      controller: 'CategoriesController'
    })
    .when('/category/:category*', {
      templateUrl: 'view/subIndex.html',
      controller:  'CategoryController'
    })
    .when('/maintainers', {
      templateUrl: 'view/maintainers.html',
      controller:  'MaintainersController'
    })
    .when('/maintainer/:maintainer*', {
      templateUrl: 'view/subIndex.html',
      controller:  'MaintainerController'
    })
    .when('/licenses', {
      templateUrl: 'view/licenses.html',
      controller:  'LicensesController'
    })
    .when('/license/:license*', {
      templateUrl: 'view/subIndex.html',
      controller:  'LicenseController'
    })
    .otherwise({ redirectTo: '/' });
}) // }}}

.factory('popover', function(){ // {{{
  var popovers = [];
  return {
    popovers: popovers,
    hide: function() {
      for (var i = 0; i < popovers.length; i ++) {
        popovers[i].popover('hide');
      }
    }
  }
}) // }}}
.factory('search', function(){ // {{{
  return {
    set: undefined
  }
}) // }}}

.directive('rankingTable', function(){ // {{{
  return {
    restrict: 'E',
    templateUrl: 'view/rankingTable.html',
    scope: {caption: '@', skip: '=', ranking: '=', tooltip: '@', more: '@', detail: '@'},
    link: function($scope, $elems){
      $scope.$watch('ranking', function(newVal){
        if(!newVal) {return;}
        if($scope.tooltip) {$($elems).find('caption').css('cursor', 'help').tooltip({title: $scope.tooltip});}
        $scope.items = $scope.ranking;
      });
    }
  }
}) // }}}
.directive('rankingTables', function(){ // {{{
  return {
    restrict: 'E',
    templateUrl: 'view/rankingTables.html'
  }
}) // }}}
.directive('loadingContents', function(){ // {{{
  return {
    restrict: 'A',
    templateUrl: 'view/loadingContents.html',
    scope: {complete: '=', error: '='},
    transclude: true
  }
}) // }}}
.directive('paginate', function(){ // {{{
  return {
    restrict: 'E',
    replace: true,
    templateUrl: 'view/paginate.html',
    scope: {itemsPerPage: '=', page: '=', numItems: '=', paginateFunction: '='},
    link: function($scope){
      $scope.$watchGroup(['numItems', 'page'], function(newVal){
        if(!newVal[0] || !newVal[1]) { return; }
        $scope.last  = Math.ceil($scope.numItems / $scope.itemsPerPage);

        if ($scope.last > 7 ) {
          if ($scope.page < 4) {
            $scope.pages = [1,2,3,4, null, $scope.last];
          } else if ($scope.page > $scope.last - 3) {
            $scope.pages = [1, null, $scope.last - 3, $scope.last - 2, $scope.last - 1, $scope.last];
          } else {
            $scope.pages = [1, null, $scope.page - 1, $scope.page, $scope.page + 1, null, $scope.last];
          }
        } else {
          $scope.pages = _.range(1, $scope.last + 1);
        }
      });
    }
  }
}) // }}}
.directive('popover', function(popover){ // {{{
  return {
    restrict: 'A',
    scope: {popoverTitle: '@', popoverContent: '@'},
    link: function($scope, $elems) {
      var elem = $($elems[0]);
      popover.popovers.push(elem);
      elem.popover({
        content: $scope.popoverContent,
        html: true,
        container: 'body',
        placement: 'bottom',
        title: $scope.popoverTitle,
        trigger: 'focus'
      });
    }
  }
}) // }}}
.directive('downloadsChart', function(){ // {{{
  return {
    restrict: 'A',
    scope: {downloads: '=', height: '@', releases: '='},
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
      function completeDownloads(dls) {
        var last   = dls[dls.length-1].date;
        var day    = dls[0].date;
        var total  = 0;
        var newdls = [];

        while (day <= last) {
          var d = _.find(dls, function(p){return (p.date - day) == 0});
          if(d) {
            total += d.count;
            newdls.push({date: day, count: d.count, total: total});
          } else {
            newdls.push({date: day, count: 0, total: total});
          }
          day = new Date(day.getTime() + 1000 * 60 * 60 * 24);
        }
        return newdls;
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

      var x  = d3.time.scale.utc().range([0, width]);
      var yt = d3.scale.linear().range([height, 0]);
      var yc = d3.scale.linear().range([height, 0]);

      var release  = [];
      var releases = [];
      var current_date = undefined;
      var max_release = 1;
      for (var i = 0; i < $scope.releases.length; i++) {
        var d = $scope.releases[i];
        var r = new Date(d.release);
        if (current_date - r == 0) {
          release.push(d.version);
        } else {
          if (current_date) {
            releases.push({date: current_date, versions: release});
          }
          max_release = Math.max(max_release, release.length);
          release = [d.version];
          current_date = r;
        }
      }
      releases.push({date: current_date, versions: release});
      max_release = Math.max(max_release, release.length);

      for (var i = 0; i < releases.length; i++) {
        plotarea.append('line')
          .attr('class', 'release line')
          .attr('y1', 0)
      }
      d3.selectAll('.release.line').data(releases);

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

      var total = d3.svg.line()
        .x(function(d){return x(d.date);})
        .y(function(d){return yt(d.total);});

      var count = d3.svg.line()
        .x(function(d){return x(d.date);})
        .y(function(d){return yc(d.count);});
 
      var xAxis  = d3.svg.axis().scale(x).orient('bottom');
      var yAxisT = d3.svg.axis().scale(yt).orient('left').ticks(10).tickFormat(countFormatter);
      var yAxisC = d3.svg.axis().scale(yc).orient('right').ticks(10).tickFormat(countFormatter);

      var countPath = plotarea.append('path').attr('class', 'line count');
      var totalPath = plotarea.append('path').attr('class', 'line total');

      var cursor = plotarea.append('line')
        .attr('class', 'cursor line')
        .attr('x1',   0)
        .attr('y1',   0)
        .attr('x2',   0)
        .attr('y2', height)
        .attr('display', 'none');

      plotarea.append('text')
        .attr('class', 'release text shadow')
        .attr('x', 0)
        .attr('y', -5);

      plotarea.append('text')
        .attr('class', 'release text')
        .attr('x', 0)
        .attr('y', -5);

      var releaseText = d3.selectAll('.release.text').attr('display', 'none');

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

        svg.selectAll('.release.line')
          .attr('x1', function(d){return x(d.date)})
          .attr('x2', function(d){return x(d.date)})
          .attr('display', function(d){return x(d.date) == 0 ? 'none' : null;})
          .attr('y2', height);


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

        countPath.attr('d', count);
        totalPath.attr('d', total);
      }
      $(window).on('orientationchange resize', updateWindow);

      var cursor_initialized = false;

      function cursorVisible (vis) {
        var v = vis && cursor_initialized ? null : 'none';
        cursor.attr('display',      v);
        inspector.attr('display',   v);
      }

      function mouseMove () {
        var size = getScopeSize();
        var mouse = d3.mouse(this);
        mouse = [mouse[0] / size.scale, mouse[1] / size.scale];
        var mx    = mouse[0] - margin.left;

        var width  = size.width  - margin.left - margin.right;
        var height = size.height - margin.top  - margin.bottom;
        if (mx < 0 || mx > width || mouse[1] > height + margin.top || mouse[1] < margin.top) {
          cursorVisible(false);
          releaseText.attr('display', 'none');
          return;
        } else { cursorVisible(true); }

        var dls   = completeDownloads($scope.downloads);
        var x0    = x.invert(mouse[0] - margin.left);
        x0        = new Date(x0.getTime() - 1000 * 60 * 60 * 12);
        var i     = dateBisector(dls, x0);
        var d0    = dls[i];
        var d1    = dls[i+1];
        var data  = d1 && x0 - d0.date > d1.date - x0 ? d1 : d0;

        var tx = mouse[0] > width  + margin.left - 100 ? mx - 100 : mx;
        var ty = mouse[1] > height + margin.top  -  50 ? mouse[1] - margin.top - 50 : mouse[1] - margin.top;
        inspector.attr('transform', 'translate(' + tx + ',' + ty + ')');
        
        inspectorDate.text(dateFormatter(data.date));
        inspectorCount.text(countFormatter(data.count));
        inspectorTotal.text(countFormatter(data.total));

        var cx = x(data.date);
        cursor.attr('transform', 'translate(' + cx + ',0)');
        cursor_initialized = true;

        var release = _.find(releases, function(r){return r.date - data.date == 0});
        if (release) {
          var t = release.versions.length > 5
            ? release.versions.slice(0,5).join(' / ') + ' ...'
            : release.versions.join(' / ');
          releaseText
            .text(t)
            .attr('display', null)
            .attr('transform', 'translate(' + cx + ',0)rotate(90)');
          cursor.attr('style', 'stroke: red');
        } else {
          releaseText.attr('display', 'none');
          cursor.attr('style', null);
        }

      }

      $scope.$watch('downloads', function(newVal){
        if(!newVal) {return;}

        var dls = completeDownloads($scope.downloads);
        x.domain(d3.extent(dls, function(d){return d.date;}));
        yt.domain([0, d3.extent(dls, function(d){return d.total;})[1]]);
        yc.domain([0, d3.extent(dls, function(d){return d.count;})[1]]);

        countPath.datum(dls);
        totalPath.datum(dls);

        plotarea.append('g')
            .attr('class', 'x axis');

        svg
          .on('mouseover', function(){cursorVisible(true)})
          .on('mouseout',  function(){cursorVisible(false)})
          .on('mousemove', mouseMove);

        updateWindow();

      });
    }
  }
}) // }}}

.controller('NavbarController', function($scope, $location, $route, popover, search){ // {{{
  search.set = function(s){$scope.query = s;}
  $scope.error = false;
  $scope.submit = function(){
    if ($scope.query && $scope.query.length > 0) {
      $location.search('page', 1);
      $location.search('mode', $scope.mode || $location.search().mode);
      $location.path('/search/' + $scope.query);
      $route.reload();
      popover.hide();
    } else {
      $scope.error = true;
    }
  };
  $scope.$watch('query', function(){$scope.error = false;});
}) // }}}
.controller("IndexController", function($rootScope, $scope, $http){ // {{{
  $rootScope.title        = "index";
  $http.get('/tables').success(function(data){
    $scope.complete   = true;
    $scope.nPackages  = data.nPackages;
    $scope.lastUpdate = data.lastUpdate;
    $scope.total      = data.total.ranking;
    $scope.weekly     = data.weekly.ranking;
    $scope.new        = data.new.ranking;
    $scope.active     = data.active.ranking;
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
    $scope.maintainers    = data.maintainers;
    $scope.name           = data.name;
    $rootScope.title      = data.name;
    $scope.packageUrl     = data.packageUrl;
    $scope.releases       = data.releases;
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
.controller('SearchController', function($rootScope, $routeParams, $scope, $http, $location, search){ // {{{
  $scope.page         = parseInt($location.search().page) || 1;
  $scope.itemsPerPage = 10;
  $scope.rawQuery     = $routeParams.query;
  $rootScope.title    = "Search:" + $scope.rawQuery;
  search.set($scope.rawQuery);

  var params = {q: [], category: [], maintainer: []};
  var query = $scope.rawQuery.split(/ +/);
  for (var i = 0; i < query.length; i++) {
    if (query[i].match(/category:/i))   {params.category.push(query[i].slice(9).replace('+', ' '));  continue}
    if (query[i].match(/maintainer:/i)) {params.maintainer.push(query[i].slice(11)); continue}
    if (query[i].match(/license:/i))    {params.license = query[i].slice(8);  continue}
    params.q.push(query[i]);
  }

  $http({method: 'GET', url: '/count', params: params}).success(function(data){
    $scope.hit = parseInt(data);
    $rootScope.title    = "Search:" + $scope.rawQuery + '(' + data + ')';
  });
  
  $scope.paginate = function(num){
    $scope.complete = false;
    $scope.page     = num;
  }

  params.limit = $scope.itemsPerPage;
  var mode = $location.search().mode;
  if      (mode == 'weekly') { $scope.mode = "last 1 week"; params['range'] = 7; }
  else if (mode == 'new')    { $scope.mode = "new packages"; params['new'] = true; }
  else if (mode == 'active') { $scope.mode = "active packages"; params['active'] = true; params['range'] = 31 }

  $scope.$watch('page', function(page) {
    $location.search('page', page);
    $scope.skip         = (page - 1) * $scope.itemsPerPage;
    params.skip = $scope.skip;
    $http({method: 'GET', url: '/ranking', params: params}).success(function(data){
      $scope.complete = true;
      if (page == $scope.page){
        $scope.result = data.ranking;
      }
    })
    .error(function(data, status){
      $scope.complete = true;
      $scope.error = {
        title: status,
        description: data
      }
    });
  });

}) // }}}
.controller('DetailController', function($rootScope, $scope, $routeParams, $location, $http){ // {{{
  var params = {};
  if      ( $routeParams.mode == 'total'  ) {$scope.denominator = "total downloads" }
  else if ( $routeParams.mode == 'weekly' ) {$scope.denominator = "last 1 week" ;    params['range']  = 7 }
  else if ( $routeParams.mode == 'new'    ) {$scope.denominator = "new packages";    params['new']    = true }
  else if ( $routeParams.mode == 'active' ) {$scope.denominator = "active packages"; params['active'] = true; params['range'] = 31 }
  else {$location.path('/');}

  $scope.category   = $location.search().category;
  $scope.maintainer = $location.search().maintainer;
  $scope.license    = $location.search().license;
  if ($scope.category)   {params['category']   = $scope.category}
  if ($scope.maintainer) {params['maintainer'] = $scope.maintainer}
  if ($scope.license)    {params['license']    = $scope.license}
  $rootScope.title = $scope.denominator
    + ($scope.category   ? " in " + $scope.category      : "")
    + ($scope.license    ? " licensed " + $scope.license : "")
    + ($scope.maintainer ? " by " + $scope.maintainer    : "");

  $scope.page         = parseInt($location.search().page) || 1;
  $scope.itemsPerPage = 10;

  $http({method: 'GET', url: '/count', params: params}).success(function(data){
    $scope.hit = parseInt(data);
  });

  $scope.paginate = function(num){
    $scope.complete = false;
    $scope.page     = num;
  }

  params['limit'] = $scope.itemsPerPage;

  $scope.$watch('page', function(page){
    $location.search('page', page);
    $scope.skip = (page - 1) * $scope.itemsPerPage;
    params['skip'] = $scope.skip;
    $http({method: 'GET', url: '/ranking', params: params}).success(function(data){
      $scope.complete = true;
      if($scope.page == page) {
        $scope.result   = data.ranking;
      }
    })
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
.controller("CategoryController", function($rootScope, $scope, $routeParams, $http){ // {{{
  var cat = $routeParams.category;
  $scope.category = cat;
  $rootScope.title = "Category:" + cat;
  $scope.title     = "Category:" + cat;
  $scope.params = "?category=" + cat;
  $http({method: "GET", url: '/tables', params: {category: cat}})
    .success(function(data){
      $scope.complete   = true;
      $scope.nPackages  = data.nPackages;
      $scope.lastUpdate = data.lastUpdate;
      $scope.total      = data.total.ranking;
      $scope.weekly     = data.weekly.ranking;
      $scope.new        = data.new.ranking;
      $scope.active     = data.active.ranking;
    }).error(function(data, status){
      $scope.complete = true;
      $scope.error = {
        title: status,
        description: data
      }
    });
}) // }}}

.controller('MaintainersController', function($rootScope, $scope, $http){ // {{{
  $rootScope.title = "maintainers";
  $http.get('/maintainers').success(function(data){
    $scope.complete    = true;
    $scope.maintainers = data.maintainers;
    var max = 0;
    for(var i = 0; i < data.maintainers.length; i++){
      max = Math.max(max, data.maintainers[i].count);
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
.controller("MaintainerController", function($rootScope, $scope, $routeParams, $http){ // {{{
  var m = $routeParams.maintainer;
  $rootScope.title = "Maintainer:" + m;
  $scope.title     = "Maintainer:" + m;
  $scope.params = "?maintainer=" + m;
  $http({method: "GET", url: '/tables', params: {maintainer: m}})
    .success(function(data){
      $scope.complete   = true;
      $scope.nPackages  = data.nPackages;
      $scope.lastUpdate = data.lastUpdate;
      $scope.total      = data.total.ranking;
      $scope.weekly     = data.weekly.ranking;
      $scope.new        = data.new.ranking;
      $scope.active     = data.active.ranking;
    }).error(function(data, status){
      $scope.complete = true;
      $scope.error = {
        title: status,
        description: data
      }
    });
}) // }}}

.controller('LicensesController', function($rootScope, $scope, $http){ // {{{
  $rootScope.title = "Licenses";
  $http.get('/licenses').success(function(data){
    $scope.complete = true;

    var other = undefined;
    var ls    = [];
    for (var i = 0; i < data.licenses.length; i++) {
      if(data.licenses[i].license != "OtherLicense") {
        ls.push(data.licenses[i]);
      } else {
        other = data.licenses[i];
      }
    }
    ls.push(other);
    $scope.licenses = ls;

    var max = 0;
    for(var i = 0; i < data.licenses.length; i++){
      max = Math.max(max, data.licenses[i].count);
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
.controller("LicenseController", function($rootScope, $scope, $routeParams, $http){ // {{{
  var l = $routeParams.license;
  $rootScope.title = "License:" + l;
  $scope.title     = "License:" + l;
  $scope.params = "?license=" + l;
  $http({method: "GET", url: '/tables', params: {license: l}})
    .success(function(data){
      $scope.complete   = true;
      $scope.nPackages  = data.nPackages;
      $scope.lastUpdate = data.lastUpdate;
      $scope.total      = data.total.ranking;
      $scope.weekly     = data.weekly.ranking;
      $scope.new        = data.new.ranking;
      $scope.active     = data.active.ranking;
    }).error(function(data, status){
      $scope.complete = true;
      $scope.error = {
        title: status,
        description: data
      }
    });
}) // }}}
