'use strict';

angular.module("bestHaskellApp", ['ngRoute'])
.config(function($routeProvider){
  $routeProvider
    .when('/', { templateUrl: 'view/index.html'
               , controller:  'IndexController'
               })
})
.directive('rankingTable', function($http){
  return {
    restrict: 'E',
    templateUrl: 'view/rankingTable.html',
    scope: true,
    link: function(scope, element, attrs, controller){
      $http.get(attrs.route)
        .success(function(data){
          console.log(data);
          scope.ranking = data.ranking;
          scope.caption = attrs.caption;
        });
    }
  }
})
.controller("IndexController", function($rootScope, $scope, $http){
  $rootScope.title     = "index";
  $scope.range_start   = null;
  $scope.range_end     = null;
  $scope.package_count = null;

  $http.get('/data/range')
    .success(function(data){
      $scope.range_start = data.start;
      $scope.range_end   = data.end;
    });

  $http.get('/data/packages/count')
    .success(function(data){
      $scope.package_count = data;
    });
})
