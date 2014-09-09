'use strict';

angular.module("bestHaskellApp", ['ngRoute'])
.config(function($routeProvider){
  $routeProvider
    .when('/', {
      templateUrl: 'view/index.html',
      controller:  'IndexController'
    })
    .when('/category/:category', {
      templateUrl: 'view/category.html',
      controller:  'CategoryController'
    });
})
.directive('rankingTable', function($http){
  return {
    restrict: 'E',
    replace: true,
    templateUrl: 'view/rankingTable.html',
    scope: {caption: '@', ranking: '='},
    link: function(scope){
      scope.$watch('ranking', function(v){
        scope.items = scope.ranking;
      });
    }
  }
})
.controller("IndexController", function($rootScope, $scope, $http){
  $rootScope.title     = "index";
  $http.get('/').success(function(data){
    $scope.nPackages  = data.nPackages;
    $scope.lastUpdate = data.lastUpdate;
    $scope.total      = data.total.ranking;
    $scope.weekly     = data.weekly.ranking;
    $scope.monthly    = data.monthly.ranking;
  });
})
.controller("CategoryController", function($rootScope, $scope, $routeParams, $http){
  var cat = $routeParams.category;
  $scope.category = cat;
  $rootScope.title = "Category:" + cat;
  $http({method: "GET", url: '/ranking', params: {category: cat}})
    .success(function(data){
      $scope.ranking = data.ranking;
    });
})
