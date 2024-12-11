/**
 * @type {import('@jest/types').Config.ProjectConfig}
 */
module.exports = {
  roots: ["<rootDir>"],
  setupFilesAfterEnv: ["./jest.setup.ts"],
  testEnvironment: "node",
  transform: {
    "^.+\\.tsx?$": "@swc/jest",
  },
};
